/* iio_nif.c --- 
 * 
 * Filename: iio_nif.c
 * Description: 
 * Author: 
 * Maintainer: 
 * Created: Wed Jun  4 22:09:30 2014 (-0700)
 * Version: 
 * URL: 
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary: 
 * 
 * 
 * 
 */

/* Change Log:
 * 
 * 
 */

/* *This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 3, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth
 * Floor, Boston, MA 02110-1301, USA.
 */

/* Code: */

#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <math.h>
#include <string.h>

#include <erl_nif.h>

#include "iio_utils.h"

/* XXX TODO
 *
 * This library will support:
 *
 * 1. init(), initialization function on a per device basis
 *   - given device number/name create/init device struct and add to list
 *   
 * 2. add_channel/rem_channel, low level call to configure channel (this must be called by an
 * erlang function which first enables or disables the channel
 * 
 *   - add(), given index/name, type, format -- init channel struct/buffer and add to list
 *   - rem(), given index/name -- remove from list
 *   
 * 3. enable_buffer/disable_buffer
 *   - erlang wrapper first enables or disables the buffer
 *   - lib call opens or closes the /dev/iio:deviceX
 *    
 */

/* NIF library resource structures */
typedef struct _libiio_dev_t {
//	unsigned int dev_num;	  /* iio device number */
	char devname[40];	  /* name of associated device */
	int trigger_num;          /* iio trigger device number */
	/* char trigname[40];	  /\* name o associated trigger *\/ */
	int fd;			  /* FD of associated device buffer */
	ErlNifMutex *dev_lock;	  /* per device lock */
	struct list_head chan_list; /* channels we have */
	struct list_head list;	  /* for linked list */
} libiio_dev_t;

typedef struct _libiio_chan_t {
	int index;
	char name[40];
	char type[20];
	uint16_t *buf;
	bfin_cirbuf_t c;
	struct list_head list;	  /* for linked list */
} libiio_chan_t;

typedef struct _libiio_chan_handle_t {
	libiio_dev_t *dev;	/* pointer to associated device */
	libiio_chan_t *chan;	/* pointer to associated channel */
} libiio_chan_handle_t;

/* resource types owned by this NIF library */
typedef struct _libiio_priv_data_t {
	ErlNifResourceType *libiio_dev_res_type;
	ErlNifResourceType *libiio_chan_res_type;
	struct list_head dev_list;
	ErlNifMutex *lock;
} libiio_priv_data_t;

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

/* frequently used terms */
static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_empty;

/* initialize a iio device */
static ERL_NIF_TERM
initialize_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	char devname[40];
	char channame[40];
	char bufdevname[80];
	char devdirpath[80];
	ERL_NIF_TERM *devinfo_term;
	ERL_NIF_TERM *chaninfo_term;
	ERL_NIF_TERM head, tail;
	int devinfo_tuple_len;
	int chaninfo_tuple_len;
	libiio_dev_t *iiodev;
	libiio_chan_t *iiochan;
	struct list_head *pos;

	/* get pointer to private data */
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)enif_priv_data(env);

	/* argument is a tuple of device information */
	/* {Name, BufDevPath, DevDirPath, ChanInfoList} */

	/* grab the tuple */
	if (!enif_get_tuple(env, argv[0], &devinfo_tuple_len, &devinfo_term)) {
		printf("libiio: expected device info tuple\n");
		return enif_make_badarg(env);
	}
	if (devinfo_tuple_len != 4) {
		printf("libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* grab the device name string */
	if (!enif_get_string(env, devinfo_tuple[0], &devname, sizeof(devname), ERL_NIF_LATIN1)) {
		printf("libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* grab the buffer device path string */
	if (!enif_get_string(env, devinfo_tuple[1], &bufdevname, sizeof(bufdevname), ERL_NIF_LATIN1)) {
		printf("libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* grab the device sysfs directory path string */
	if (!enif_get_string(env, devinfo_tuple[2], &devdirpath, sizeof(devdirpath), ERL_NIF_LATIN1)) {
		printf("libiio: invalid device tuple\n");
		return enif_make_badarg(env);
	}

	/* grab channel info list */
	if (enif_is_empty_list(env, devinfo_tuple[3])) {
		fprintf(stderr, "libiio: expected non-empty channel info list\n");
		return enif_make_badarg(env);
	}

	/* check if this device is already initialized */
	enif_mutex_lock(priv->lock);
	if (!list_empty(&priv->dev_list)) {
		list_for_each(pos, &priv->dev_list) {
			iohandle = list_entry(pos, libiio_handle_t, list);
			if (!strcmp(iohandle->devname, devname)) {
				/* matched */
				enif_mutex_unlock(priv->lock);
				return enif_make_tuple2(env,
							atom_error,
							mk_atom(env, "already_open"));
			}
		}
	}
	enif_mutex_unlock(priv->lock);

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

	/* init trigger number */
	iiodev->trigger_num = -1;

	/* init FD */
	iiodev->fd = -1;

	/* create the device lock */
	iiodev->dev_lock = enif_mutex_create("dev_lock");
	if (iiodev->dev_lock == NULL) {
		fprintf(stderr, "libiio: failed to create device mutex\n");
		return -1;
	}

        /* init channel list */
	INIT_LIST_HEAD(&iiodev->chan_list);

	/* now grab channel info and build the list */
	
	/* grab the first element */
	if (!enif_get_list_cell(env, devinfo_tuple[3], &head, &tail)) {
		fprintf(stderr, "libiio: expected valid list\n");
		return enif_make_badarg(env);
	}
	do {
		/* grab the channel info tuple */
		/* channel info tuple is of form */
		/* {Name, Index} */
		if (!enif_get_tuple(env, head, &chaninfo_tuple_len, &chaninfo_term)) {
			printf("libiio: expected channel info tuple\n");
			return enif_make_badarg(env);
		}

		/* allocate a channel struct */
		iiochan = enif_alloc(sizeof(libiio_chan_t));
		if (iiochan == NULL) {
			goto error_free_all;
		}
		memset(iiochan, 0, sizeof(libiio_chan_t));

		/* grab name string from tuple */
		if (!enif_get_string(env, head, iiochan->name, sizeof(iiochan->name), ERL_NIF_LATIN1)) {
			printf("libiio: invalid channel info tuple\n");
			goto error_free_all;
			//return enif_make_badarg(env);
		}

		/* grab index from tuple */
		if (!enif_get_uint(env, head, &iiochan>index)) {
			fprintf(sterr, "libiio: invalid channel info tuple\n");
			goto error_free_all;
			//return enif_make_badarg(env);
		}

		/* initialize the channel buffer */
		/* default to only hold 1 sample */
		iiochan->buf = enif_alloc(1*sizeof(uint16_t));
		bfin_circbuf_init(&iiochan->c, iiochan->buf, 1*sizeof(uint16_t));

		/* add it to the channel list */
		enif_mutex_lock(iiodev->dev_lock);
		list_add(&iiodev->list, &iiodev->chan_list);
		enif_mutex_unlock(iiodev->dev_lock);
		
		/* get next element */
	} while(enif_get_list_cell(env, tail, &head, &tail));

	/* open the buffer device */
	iiodev->fd = open(devname, O_RDONLY); 
	if (fd < 0) {
		goto error_free_all;
		
	}

	/* add this new device to the dev list */
	enif_mutex_lock(priv->lock);
	list_add(&iiodev->list, &priv->dev_list);
	enif_mutex_unlock(priv->lock);

	/* create the resource handle term for this IO interface */
	iohandle_term = enif_make_resource(env, iohandle);

	/* all done */
	return atom_ok;

error_free_all:
	/* loop through channel list and free any channels and buffers */
	enif_mutex_lock(iiodev->dev_lock);
	if (!list_empty(&iiodev->chan_list)) {
		list_for_each_safe(pos, q, &iiodev->chan_list) {
			iiochan = list_entry(pos, libiio_chan_t, list);

			/* remove from list */
			list_del(pos);

			/* free any allocated buffer */
			if (iiochan->buf) {
				enif_free(iiochan->buf);
			}
			
			enif_free(iiochan);
		}
	}
	enif_mutex_unlock(iiodev->dev_lock);

	/* destroy iiodev mutex */
	enif_mutex_destroy(iiodev->dev_lock);

	enif_free(iiodev);

	return mk_errno(env, errno);
}

/* alloc_chandle_nif */
/* set up our resource */
/* chandle = enif_alloc_resource(priv->libiio_chan_res_type,  */
/* 			      sizeof(libiio_chan_handle_t));  */
/* if (chandle == NULL) { */
/* 	fprintf(stderr, "error: failed to allocate channel resource object\n"); */
/* 	return atom_error; */
/* } */
/* chandle->dev = iiodev; *//*iiodev found by name search */
/* chandle->chan = iiochan;*//*chan found by name search */
/* create the resource handle term for this IO interface */
/* iohandle_term = enif_make_resource(env, iohandle); */

/* /\* make tuple { ok, Handle } and return it *\/ */
/* return enif_make_tuple2(env,  */
/* 			atom_ok, */
/* 			iohandle_term); */


/* free_chandle_nif */
/* release this resource */
// FIXME: where does this belong? (or maybe just do right after alloc?)
//enif_release_resource(chandle);

/* XXX TODO implement reload() and upgrade() */

/* Loads the NIF module and initializes private data */
static int load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
	libserial_priv_data_t *priv;

	atom_ok = mk_atom(env, "ok");
	atom_error = mk_atom(env, "error");
	atom_empty = mk_atom(env, "rx_queue_empty");

	/* setup private data */
	if (!(priv = enif_alloc(sizeof(libiio_priv_data_t)))) {
		fprintf(stderr, "libiio: failed to allocate private data!\n");
		return -1;
	}

	/* FIXME: remove if not needed */
	/* create resource types */
	priv->libiio_dev_res_type = 
		enif_open_resource_type(env, 
					NULL/*module name not used*/, 
					"iio_device_handle", 
					NULL /*no DTOR needed*/, 
					ERL_NIF_RT_CREATE, 
					NULL/*no needed*/);
	if (!priv->libiio_dev_res_type) {
		fprintf(stderr, "libiio: failed to open device resource type\n");
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

	/* create the list lock */
	priv->lock = enif_mutex_create("lock");
	if (priv->lock == NULL) {
		fprintf(stderr, "libiio: failed to create list mutex\n");
		return -1;
	}

	*priv_data = priv;

	return 0;
}

static void unload(ErlNifEnv *env, void *priv_data)
{

	libiio_dev_t *iiodev;
	struct list_head *pos,*pos2,*q,*q2;
	void* resp;
	libiio_priv_data_t *priv = 
		(libiio_priv_data_t *)priv_data;

	/* go through the IIO device list and close/free everything */
	enif_mutex_lock(priv->lock);
	if (!list_empty(&priv->dev_list)) {
		list_for_each_safe(pos, q, &priv->dev_list) {
			iiodev = list_entry(pos, libiio_dev_t, list);
			list_del(pos); /* remove it from the list */

			enif_mutex_lock(iiodev->dev_lock);
			if (!list_empty(&iiodev->chan_list)) {
				list_for_each_safe(pos2, q2, &iiodev->chan_list) {
					iiochan = list_entry(pos2, libiio_chan_t, list);

					/* remove from list */
					list_del(pos2);

					/* free any allocated buffer */
					if (iiochan->buf) {
						enif_free(iiochan->buf);
					}
					
					enif_free(iiochan);
				}
			}
			close(iiodev->fd);
			enif_mutex_unlock(iiodev->lock);
			enif_mutex_destroy(iiodev->lock);
			enif_free(iiodev);
		}
	}
	enif_mutex_unlock(priv->lock);

	/* destroy the list lock */
	enif_mutex_destroy(priv->lock);

	/* now free the private data */
	enif_free(priv);
}

static ErlNifFunc nif_funcs[] = {
	{ "initialize", 1, initialize_nif },
};

ERL_NIF_INIT(iio, nif_funcs, load, NULL, NULL, unload)

/* iio_nif.c ends here */
