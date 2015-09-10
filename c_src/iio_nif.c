/* iio_nif.c --- 
 * 
 * Filename: iio_nif.c
 */

/* Code: */

#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

#include <erl_nif.h>
#include <erl_driver.h>

#include "list.h"
#include "bfin_circbuf.h"

/* NIF library structures */

typedef struct _libiio_chan_t {
	uint32_t enable;
	uint32_t index;
	uint32_t size;
	char name[40];
	char type[20];
	uint16_t *buf;
	bfin_circbuf_t c;
} libiio_chan_t;

typedef struct _libiio_dev_t {
	uint32_t buf_enable;
	uint32_t buf_length;
	uint32_t scan_size;
	uint8_t *readbuf;
	char devname[40];       /* name of associated device */
	int fd;                 /* FD of associated device buffer */
#ifdef _DTHREAD
	ErlNifMutex *lock;      /* per device lock */
#endif
	libiio_chan_t *chan;    /* channel array for this device */
	uint32_t chan_count;
	struct list_head list;  /* for linked list */
} libiio_dev_t;

typedef struct _libiio_chan_handle_t {
	libiio_dev_t *dev;	/* pointer to associated device */
	libiio_chan_t *chan;	/* pointer to associated channel */
} libiio_chan_handle_t;

/* resource types owned by this NIF library */
typedef struct _libiio_priv_data_t {
	ErlNifResourceType *libiio_chan_res_type;
	struct list_head dev_list;
#ifdef _DTHREAD
	ErlNifMutex *lock;
#endif
} libiio_priv_data_t;

/* frequently used terms */
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_empty;

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
	ERL_NIF_TERM ret;
	
	if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
	{
		return enif_make_atom(env, atom);
	}
	
	return ret;
}

static ERL_NIF_TERM
mk_errno(ErlNifEnv *env, int errnum)
{
	return enif_make_tuple2(env, atom_error,
				enif_make_atom(env, erl_errno_id(errnum)));
}


/* initialize a iio device */
static ERL_NIF_TERM
initialize_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	const ERL_NIF_TERM *devinfo_term;
	const ERL_NIF_TERM *chaninfo_term;
	ERL_NIF_TERM head, tail;
	int devinfo_term_len;
	int chaninfo_tuple_len;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;
	struct list_head *pos;
	int cidx;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* argument is a tuple of device information */
	/* TODO: get rid of these paths */
	/* {Name, BufDevPath, DevDirPath, ChanInfoList} */

	/* grab the tuple */
	if (!enif_get_tuple(env, argv[0], &devinfo_term_len, &devinfo_term)) {
		fprintf(stderr, "libiio: expected device info tuple\n");
		return enif_make_badarg(env);
	}
	if (devinfo_term_len != 4) {
		fprintf(stderr, "libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* grab the device name string */
	if (!enif_get_string(env, devinfo_term[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* check if this device is already initialized */
#ifdef _DTHREAD
	enif_mutex_lock(priv->lock);
#endif
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
#ifdef _DTHREAD
				enif_mutex_unlock(priv->lock);
#endif
				return enif_make_tuple2(env,
							atom_error,
							mk_atom(env, "already_open"));
			}
		}
	}
#ifdef _DTHREAD
	enif_mutex_unlock(priv->lock);
#endif

	/* not already opened, good... */

	/* allocate and initialize device struct */
	iiodev = enif_alloc(sizeof(libiio_dev_t));
	if (iiodev == NULL) {
		fprintf(stderr, "libiio: failed to allocate device structure\n");
		return mk_atom(env, "alloc_failed");
	}
	memset(iiodev, 0, sizeof(libiio_dev_t));

	/* copy in the device name */
	strcpy(iiodev->devname, devname);

	/* init FD */
	iiodev->fd = -1;

	/* not enabled */
	iiodev->buf_enable = 0;
	
	/* default to assume 64 16-bit samples + timestamps -- this
	 * value affects the maximum amount of data we are going to
	 * attempt to read from the buffer device when channel_read()
	 * is called */

	//iiodev->sample_size = sizeof(uint16_t); /* only 16-bit max for now */
	iiodev->scan_size = 0;//2*sizeof(uint16_t) + 1*sizeof(uint64_t);
	iiodev->buf_length = 0;//64 * iiodev->scan_size;
	iiodev->readbuf = NULL;

	/* create the device lock */
#ifdef _DTHREAD
	iiodev->lock = enif_mutex_create("lock");
	if (iiodev->lock == NULL) {
		fprintf(stderr, "libiio: failed to create device mutex\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "mutex_create"));
	}
#endif

	/* now grab channel info and build the list */
	if (!enif_get_list_length(env, devinfo_term[3], &iiodev->chan_count)) {
		fprintf(stderr, "libiio: expected channel info list\n");
		return enif_make_badarg(env);
	}	
	if (iiodev->chan_count == 0) {
		fprintf(stderr, "libiio: expected non-empty channel info list\n");
		return enif_make_badarg(env);
	}		

	/* allocate channel array */
	iiochan = enif_alloc(sizeof(libiio_chan_t) * iiodev->chan_count);
	if (iiochan == NULL) {
		goto error_free_all;
	}
	memset(iiochan, 0, sizeof(libiio_chan_t) * iiodev->chan_count);
	
	
	/* grab the first element */
	if (!enif_get_list_cell(env, devinfo_term[3], &head, &tail)) {
		fprintf(stderr, "libiio: expected valid list\n");
		return enif_make_badarg(env);
	}
	
	cidx = 0;
	do {
		/* grab the channel info tuple */
		/* channel info tuple is of form */
		/* {Name, Index} */
		if (!enif_get_tuple(env, head, &chaninfo_tuple_len, &chaninfo_term)) {
			fprintf(stderr, "libiio: expected channel info tuple\n");
			return enif_make_badarg(env);
		}

		/* grab name string from tuple */
		if (!enif_get_string(env, chaninfo_term[0], iiochan[cidx].name, sizeof(iiochan[cidx].name), ERL_NIF_LATIN1)) {
			fprintf(stderr, "libiio: invalid channel info tuple\n");
			goto error_free_all;
		}

		/* grab index from tuple */
		if (!enif_get_uint(env, chaninfo_term[1], &iiochan[cidx].index)) {
			fprintf(stderr, "libiio: invalid channel info tuple\n");
			goto error_free_all;
		}

		/* initialize the channel buffer */
		/* default to only hold 1 sample */
		iiochan[cidx].buf = enif_alloc(1*sizeof(uint16_t));
		bfin_circbuf_init(&iiochan[cidx].c, iiochan[cidx].buf, 1*sizeof(uint16_t));

		/* default to disabled */
		iiochan[cidx].enable = 0;

		cidx++;

		/* get next element */
	} while(enif_get_list_cell(env, tail, &head, &tail));

	/* put the created channel array into the device structure */
	iiodev->chan = iiochan;

	/* add this new device to the dev list */
#ifdef _DTHREAD
	enif_mutex_lock(priv->lock);
#endif
	list_add(&iiodev->list, &priv->dev_list);
#ifdef _DTHREAD
	enif_mutex_unlock(priv->lock);
#endif

	/* all done */
	return atom_ok;

error_free_all:
	if (iiodev->chan) {
		for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
			if (iiodev->chan[cidx].buf) {
				enif_free(iiodev->chan[cidx].buf);
			}
		}
		enif_free(iiodev->chan);
	}
	
	/* destroy iiodev mutex */
#ifdef _DTHREAD
	enif_mutex_destroy(iiodev->lock);
#endif

	enif_free(iiodev);

	return mk_errno(env, errno);
}

/* enable an iio device channel */
static ERL_NIF_TERM
channel_enable_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	char channame[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;
	int found = 0;
	int cidx;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* grab the chan name string */
	if (!enif_get_string(env,  argv[1], channame, sizeof(channame), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected channel name string\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	/* find the requested channel */
	found = 0;
	for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
		if (!strcmp(iiodev->chan[cidx].name, channame)) {
			/* matched */
			iiochan = &iiodev->chan[cidx];
			found = 1;
			break;
		}
	}
	if (!found) {
		fprintf(stderr, "no matching channel found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_channel_match"));
	}

	if (iiochan->enable) {
		fprintf(stderr, "channel already enabled\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "channel_already_enabled"));
	}

	/* mark it enabled */
	iiochan->enable = 1;

	/* FIXME: grab this info from the chaninfo array once supported */
	iiochan->size = sizeof(uint16_t);

	/* account for this additional channel in the scan results */
	iiodev->scan_size += iiochan->size;

	return atom_ok;
}

/* disable an iio device channel */
static ERL_NIF_TERM
channel_disable_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	char channame[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;
	int found = 0;
	int cidx;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* grab the chan name string */
	if (!enif_get_string(env,  argv[1], channame, sizeof(channame), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected channel name string\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	/* find the requested channel */
	found = 0;
	for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
		if (!strcmp(iiodev->chan[cidx].name, channame)) {
			/* matched */
			iiochan = &iiodev->chan[cidx];
			found = 1;
			break;
		}
	}
	if (!found) {
		fprintf(stderr, "no matching channel found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_channel_match"));
	}

	if (!iiochan->enable) {
		fprintf(stderr, "channel not enabled\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "channel_not_enabled"));
	}

	/* mark it disabled */
	iiochan->enable = 0;

	/* account for this channel being removed from the scan results */
	iiodev->scan_size -= iiochan->size;

	/* no size, not enabled */
	iiochan->size = 0;

	return atom_ok;
}

/* get a handle to an iio device channel */
static ERL_NIF_TERM
channel_get_handle_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	char channame[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;
	libiio_chan_handle_t *chandle; 
	int found = 0;
	int cidx;
	ERL_NIF_TERM chandle_term;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* grab the chan name string */
	if (!enif_get_string(env,  argv[1], channame, sizeof(channame), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected channel name string\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	/* find the requested channel */
	found = 0;
	for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
		if (!strcmp(iiodev->chan[cidx].name, channame)) {
			/* matched */
			iiochan = &iiodev->chan[cidx];
			found = 1;
			break;
		}
	}
	if (!found) {
		fprintf(stderr, "no matching channel found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_channel_match"));
	}

	/* make sure it is enabled */
	if (!iiochan->enable) {
		fprintf(stderr, "channel not enabled!\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "channel_not_enabled"));
	}		

	/* set up our resource (handle) */
	chandle = enif_alloc_resource(priv->libiio_chan_res_type,
				      sizeof(libiio_chan_handle_t));
	if (chandle == NULL) {
		fprintf(stderr, "error: failed to allocate channel resource object\n");
		return atom_error;
	}
	chandle->dev = iiodev;
	chandle->chan = iiochan;

        /* create the resource handle term for this IO interface */
        chandle_term = enif_make_resource(env, chandle);
	
	/* release it so it can be garbage collected by the VM when no
	 * longer used. */
	enif_release_resource(&chandle_term);

        /* make tuple { ok, Handle } and return it */
	return enif_make_tuple2(env,
				atom_ok,
				chandle_term);
}

/* set iio device channel userspace circular buffer length */
static ERL_NIF_TERM
channel_buffer_set_length_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	uint32_t new_len;
	libiio_chan_handle_t *chandle; 
	
	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* first arg is the io handle */
	if (!enif_get_resource(env, 
			       argv[0], 
			       priv->libiio_chan_res_type, (void **)&chandle)) {
		fprintf(stderr, "libiio: expected channel handle as first arg\n");
		return enif_make_badarg(env);
	}

	/* second arg is the buf len */
	if (!enif_get_uint(env, argv[1], &new_len)) {
		fprintf(stderr, "libiio: expected uint length as second arg\n");
		return enif_make_badarg(env);
	}

#ifdef _DTHREAD
	enif_mutex_lock(chandle->dev->lock);
#endif

	/* resize the circular buffer and re-init */
	enif_free(chandle->chan->buf);
	chandle->chan->buf = enif_alloc(sizeof(uint16_t) * new_len);
	if (!chandle->chan->buf) {
		fprintf(stderr, "libiio: failed to allocate channel buffer\n");
		return mk_atom(env, "alloc_failed");
	}

	bfin_circbuf_init(&chandle->chan->c, chandle->chan->buf, sizeof(uint16_t) * new_len);

#ifdef _DTHREAD
	enif_mutex_unlock(chandle->dev->lock);
#endif

	/* all done */
	return atom_ok;
}

/* Read channel data */
static ERL_NIF_TERM
channel_read_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	uint32_t len;
	ErlNifBinary bin_term;
	libiio_chan_handle_t *chandle; 
	uint16_t *p;
	uint16_t *pr;
	int i,j, cidx;
	int total;
	int read_size;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* first arg is the io handle */
	if (!enif_get_resource(env, 
			       argv[0], 
			       priv->libiio_chan_res_type, (void **)&chandle)) {
		fprintf(stderr, "libiio: expected channel handle as first arg\n");
		return enif_make_badarg(env);
	}
	iiodev = chandle->dev;
	iiochan = chandle->chan;

	/* second arg is the read len (in samples) */
	if (!enif_get_uint(env, argv[1], &len)) {
		fprintf(stderr, "libiio: expected uint length as second arg\n");
		return enif_make_badarg(env);
	}

	if (!iiodev->buf_enable) {
		fprintf(stderr, "buffer not enabled!\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "buffer_not_enabled"));
	}

	if (!iiochan->enable) {
		fprintf(stderr, "channel not enabled!\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "channel_not_enabled"));
	}

	/* allocate binary of requested size */
	if (!enif_alloc_binary(len * iiochan->size, &bin_term)) {
		fprintf(stderr, "libiio: failed to allocate binary\n");
		return enif_make_atom(env, "alloc_failure");
	}

#ifdef _DTHREAD
	/* lock the device */
	enif_mutex_lock(iiodev->lock);
#endif

	/* read what we can from the circular buffer */
	total = 0;
	p = (uint16_t *)bin_term.data;
	for (i = 0; i < len; i++) {
		if (bfin_circbuf_read(&iiochan->c, p)) {
			break;	/* went empty */
		}
		p++;		/* only increment if read was OK */
	}
	total += i;
	if (total < len) {
		/* didn't have enough, go get more */
		read_size = read(iiodev->fd, iiodev->readbuf, iiodev->buf_length * iiodev->scan_size);
		if ((read_size < 0) && (errno != EAGAIN)) {
			fprintf(stderr, "libiio: read error\n");
#ifdef _DTHREAD
			enif_mutex_unlock(iiodev->lock);
#endif
			enif_release_binary(&bin_term);
			return mk_errno(env, errno);
		} else if (read_size < 0) { /* EAGAIN */
			/* kernel buffer is empty, assume we're done
			 * for now */
			goto done;
		}

		/* demux the channel data we read into the enabled
		 * channel buffers */
		/* FIXME: THIS WILL NOT SUPPORT TIMESTAMPS! */
		/* FIXME: this only supports 2-byte sample size */
		pr = (uint16_t *)iiodev->readbuf;
		for (j = 0; j < read_size/iiodev->scan_size; j++) {
			for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
				/* only enabled channels are in the scan results */
				if (iiodev->chan[cidx].enable) {
					bfin_circbuf_write(&iiodev->chan[cidx].c, *pr++);
				}
			}
		}

		/* ok, now copy in more data from circular buffer
		 * until satified */
		for (; i < len; i++) {
			if (bfin_circbuf_read(&iiochan->c, p))
				break;	/* went empty */
			p++;		/* only increment if read was OK */
		}
		total += i;
	}

done:
#ifdef _DTHREAD
	/* unlock */
	enif_mutex_unlock(chandle->dev->lock);
#endif

	if (total == 0) {
		enif_release_binary(&bin_term);
		return atom_empty;
	} else if (total < len) {
		/* resize the binary to fit the data */
		enif_realloc_binary(&bin_term, total * iiochan->size);
	}
	return enif_make_tuple2(env, atom_ok, enif_make_binary(env, &bin_term));
}


/* enable an iio device buffer */
static ERL_NIF_TERM
buffer_enable_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	int found = 0;
	char bufdev[40];

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* grab the buffer device file string */
	if (!enif_get_string(env, argv[1], bufdev, sizeof(bufdev), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected buffer device file string\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	if (iiodev->buf_enable) {
		fprintf(stderr, "buffer already enabled\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "buffer_already_enabled"));
	}

	if (!iiodev->scan_size) {
		fprintf(stderr, "no channels have been enabled!\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_channel_enabled"));
	}

	/* at this point any channels which will be in the scan have
	 * been enabled and scan_size is set. We can now allocate a
	 * read buffer. */

#ifdef _DTHREAD
	enif_mutex_lock(iiodev->lock);
#endif
	
	/* free any old buffer if it exists */
	if (iiodev->readbuf) {
		enif_free(iiodev->readbuf);
	}

	/* allocate enough to hold all enabled channels */
	iiodev->readbuf = enif_alloc(iiodev->buf_length * iiodev->scan_size);
	if (!iiodev->readbuf) {
		fprintf(stderr, "libiio: failed to allocate read buffer\n");
#ifdef _DTHREAD
		enif_mutex_unlock(iiodev->lock);
#endif
		return mk_atom(env, "alloc_failed");
	}
	
	/* open the device */
	iiodev->fd = open(bufdev, O_RDONLY);
	if (iiodev->fd < 0) {
		/* if (errno == ENOENT) { */
		/* 	/\* try making the device file *\/ */
		/* 	mknod(bufdev,  */
		/* 	      S_IFCHR | S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH,  */
		/* 	      makedev(iiodev->major, iiodev->minor));     */

		fprintf(stderr, "failed to open the device: %s\n", strerror(errno));
#ifdef _DTHREAD
		enif_mutex_unlock(iiodev->lock);
#endif
		return mk_errno(env, errno);
	}

	/* mark enabled */
	iiodev->buf_enable = 1;

#ifdef _DTHREAD
	enif_mutex_unlock(iiodev->lock);
#endif
	return atom_ok;
}

/* set iio device buffer length (actually the kernel buffer is set up
 * in erlang by a sysfs file write, but down here in the library we
 * adjust our read buffer accordingly) */
static ERL_NIF_TERM
buffer_set_length_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	int found = 0;
	uint32_t len;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* grab the buffer device file string */
	if (!enif_get_uint(env, argv[1], &len)) {
		fprintf(stderr, "libiio: expected buffer length\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	/* can't change it while in use */
	if (iiodev->buf_enable) {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "device_busy"));
	}

	/* we set the new length in the length in the device struct,
	 * but cannot allocate the buffer yet -- we do that when the
	 * enable call is made, after channels have been enabled so we
	 * know the scan chunk size */

#ifdef _DTHREAD
	enif_mutex_lock(iiodev->lock);
#endif
	
	/* free any old buffer if it exists */
	if (iiodev->readbuf) {
		enif_free(iiodev->readbuf);
	}

	/* set new length */
	iiodev->buf_length = len;

	/* allocate enough to hold all channels (should they all get enabled) */
	iiodev->readbuf = enif_alloc(iiodev->buf_length * iiodev->scan_size);
	if (!iiodev->readbuf) {
		fprintf(stderr, "libiio: failed to allocate read buffer\n");
#ifdef _DTHREAD
		enif_mutex_unlock(iiodev->lock);
#endif
		return mk_atom(env, "alloc_failed");
	}
	
#ifdef _DTHREAD
	enif_mutex_unlock(iiodev->lock);
#endif

	/* all done */
	return atom_ok;
}

/* disable an iio device buffer */
static ERL_NIF_TERM
buffer_disable_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	struct list_head *pos;
	libiio_dev_t *iiodev;
	int found = 0;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* grab the device name string */
	if (!enif_get_string(env, argv[0], devname, sizeof(devname), ERL_NIF_LATIN1)) {
		fprintf(stderr, "libiio: expected device name string\n");
		return enif_make_badarg(env);
	}

	/* find the requested device */
	found = 0;
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			if (!strcmp(iiodev->devname, devname)) {
				/* matched */
				found = 1;
				break;
			}
		}
	} else {
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_devices_initialized"));
	}
	if (!found) {
		fprintf(stderr, "no matching device found\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "no_device_match"));
	}

	if (!iiodev->buf_enable) {
		fprintf(stderr, "buffer not enabled\n");
		return enif_make_tuple2(env,
					atom_error,
					mk_atom(env, "buffer_not_enabled"));
	}

#ifdef _DTHREAD
	enif_mutex_lock(iiodev->lock);
#endif
	
	/* mark disabled */
	iiodev->buf_enable = 0;
	
	/* free read buffer if it exists */
	if (iiodev->readbuf) {
		enif_free(iiodev->readbuf);
	}
	iiodev->readbuf = NULL;

	/* close the device */
	close(iiodev->fd);

#ifdef _DTHREAD
	enif_mutex_unlock(iiodev->lock);
#endif
	return atom_ok;
}

/* XXX TODO implement reload() and upgrade() */

/* Loads the NIF module and initializes private data */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	libiio_priv_data_t *priv;

	atom_ok = mk_atom(env, "ok");
	atom_error = mk_atom(env, "error");
	atom_empty = mk_atom(env, "rx_queue_empty");

	/* setup private data */
	if (!(priv = enif_alloc(sizeof(libiio_priv_data_t)))) {
		fprintf(stderr, "libiio: failed to allocate private data!\n");
		return -1;
	}

	priv->libiio_chan_res_type = 
		enif_open_resource_type(env, 
					NULL/*module name not used*/, 
					"iio_channel_handle", 
					NULL /*no DTOR needed*/, 
					ERL_NIF_RT_CREATE, 
					NULL/*no needed*/);
	if (!priv->libiio_chan_res_type) {
		fprintf(stderr, "libiio: failed to open channel resource type\n");
		return -1;
	}

	/* init device handle list */
	INIT_LIST_HEAD(&priv->dev_list);

	/* create the device lock */
#ifdef _DTHREAD
	priv->lock = enif_mutex_create("iio_nif_dev_lock");
	if (priv->lock == NULL) {
		fprintf(stderr, "libiio: failed to create device mutex\n");
		return -1;
	}
#endif
	*priv_data = priv;

	return 0;
}

static void unload(ErlNifEnv *env, void *priv_data)
{

	libiio_dev_t *iiodev;
	//libiio_chan_t *iiochan;
	struct list_head *pos,/**pos2,*/*q/*,*q2*/;
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)priv_data;
	int cidx;

	/* go through the IIO device list and close/free everything */
#ifdef _DTHREAD
	enif_mutex_lock(priv->lock);
#endif
	if (!list_empty(&priv->dev_list)) {
		list_for_each_safe(pos, q, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			list_del(pos); /* remove it from the list */

			if (iiodev->chan) {
				for (cidx = 0; cidx < iiodev->chan_count; cidx++) {
					if (iiodev->chan[cidx].buf) {
						enif_free(iiodev->chan[cidx].buf);
					}
				}
				enif_free(iiodev->chan);
			}

			close(iiodev->fd);
#ifdef _DTHREAD
			enif_mutex_destroy(iiodev->lock);
#endif
			enif_free(iiodev);
		}
	}
#ifdef _DTHREAD
	enif_mutex_unlock(priv->lock);

	/* destroy the list lock */
	enif_mutex_destroy(priv->lock);
#endif
	/* now free the private data */
	enif_free(priv);
}

static ErlNifFunc nif_funcs[] = {
	{ "initialize", 1, initialize_nif },
	{ "channel_enable", 2, channel_enable_nif },
	{ "channel_disable", 2, channel_disable_nif },
	{ "channel_get_handle", 2, channel_get_handle_nif },
	{ "channel_buffer_set_length", 2, channel_buffer_set_length_nif },
	{ "channel_read", 2, channel_read_nif },
	{ "buffer_enable", 2, buffer_enable_nif },
	{ "buffer_disable", 1, buffer_disable_nif },
	{ "buffer_set_length", 2, buffer_set_length_nif },
};

ERL_NIF_INIT(iio_nif, nif_funcs, load, NULL, NULL, unload)

/* iio_nif.c ends here */
