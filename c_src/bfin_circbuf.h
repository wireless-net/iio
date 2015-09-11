/* bfin_circbuf.h --- 
 * Author: Lumenosys Robotics
 * Created: Sun Jan 11 11:03:24 2015 (-0800)
 */

/* Code: */
#ifndef __BFIN_CIRCBUF_H
#define __BFIN_CIRCBUF_H

#include <stdlib.h>
#include <stdint.h>

typedef struct bfin_circbuf {
	uint16_t *pwrite;
	uint16_t *pread;
	size_t len;
	uint16_t *buf;
} bfin_circbuf_t;

int bfin_circbuf_init(bfin_circbuf_t *pcircb, uint16_t *buf, int len)
{
	pcircb->buf = buf;
	pcircb->len = len;
	pcircb->pwrite = pcircb->pread = &pcircb->buf[0];
	return 0;
}

int bfin_circbuf_write(bfin_circbuf_t *pcircb, uint16_t val)
{
	/* write value */
	*pcircb->pwrite = val;

	/* update write pointer */
	pcircb->pwrite = __builtin_bfin_circptr (pcircb->pwrite, 
						 sizeof *pcircb->pwrite, 
						 pcircb->buf, 
						 pcircb->len);
	/* indicate if buffer is now full */
	if (pcircb->pwrite == pcircb->pread)
		return -1;

	return 0;
}

int bfin_circbuf_read(bfin_circbuf_t *pcircb, uint16_t *val)
{
	if (pcircb->pread == pcircb->pwrite)
		return -1;
	else
		*val = *pcircb->pread;

	pcircb->pread = __builtin_bfin_circptr (pcircb->pread, 
						sizeof *pcircb->pread, 
						pcircb->buf, 
						pcircb->len);
	return 0;
}

#endif/*__BFIN_CIRCBUF_H*/
/* bfin_circbuf.h ends here */
