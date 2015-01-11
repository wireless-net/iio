/* bfin_circbuf.c --- 
 * 
 * Filename: bfin_circbuf.c
 * Description: 
 * Author: Devin Butterfield
 * Maintainer: 
 * Created: Fri Jan  9 10:27:27 2015 (-0800)
 * Version: 
 * Last-Updated: Fri Jan  9 11:40:56 2015 (-0800)
 *           By: Devin Butterfield
 *     Update #: 33
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
#include <stdlib.h>
#include <stdint.h>

uint16_t in[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
uint16_t out[] = { 0, 0, 0, 0, 0 };

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
	

int main ()
{
    int i;
    uint16_t val;
    bfin_circbuf_t c;

    bfin_circbuf_init(&c, out, sizeof(out));

    /* write data into circular buffer */
    for (i = 0; i < (sizeof(in)/2); i++) {
	    bfin_circbuf_write(&c, in[i]);
    }

    /* read back out data in buffer */
    printf("buf = ");
    while(!bfin_circbuf_read(&c, &val)) {
	    printf("0x%x ", val);
    }
    printf("\n");

    return 0;
}

/*
 * NOTES:
 *
 * for NIF read:
 *
 * 1. use handle to index into the channel array for the device
 *
 * 2. allocate binary of requested chunk size
 *
 * 3. empty channel circ buf into binary up to request size or until
 * buf goes empty
 *
 * 4. if buf goes empty and request not met, do device read into
 * tmpbuf, demux and write into channel circbuf until it goes full.
 *
 * 5. repeat steps 3...
 */

/* bfin_circbuf.c ends here */
