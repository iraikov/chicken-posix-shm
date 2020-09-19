
;;
;; Chicken Scheme interface to the POSIX Shared Memory API.
;;
;; Copyright 2011-2020 Ivan Raikov.
;;
;; Based in part on code from the Ocamlnet library.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;; 
;;  - Redistributions of source code must retain the above copyright
;;  notice, this list of conditions and the following disclaimer.
;; 
;;  - Redistributions in binary form must reproduce the above
;;  copyright notice, this list of conditions and the following
;;  disclaimer in the documentation and/or other materials provided
;;  with the distribution.
;; 
;;  - Neither name of the copyright holders nor the names of its
;;  contributors may be used to endorse or promote products derived
;;  from this software without specific prior written permission.
;; 
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
;;  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;  POSSIBILITY OF SUCH DAMAGE.


(module posix-shm

 (posix-shm? shm-open shm-unlink)

 (import scheme (chicken base) (chicken foreign) (chicken blob)
         (only (chicken string) ->string)
         (only srfi-1 filter)
         (only (chicken file posix) perm/irwxu perm/irgrp perm/iroth
               open/rdonly open/rdwr open/creat open/excl open/trunc)
	  )

; Include into generated code, but don't parse:
#>

#include <errno.h>
#include <stdio.h>
static void chicken_panic (C_char *) C_noret;
static void chicken_panic (C_char *msg)
{
  C_word *a = C_alloc (C_SIZEOF_STRING (strlen (msg)));
  C_word scmmsg = C_string2 (&a, msg);
  C_halt (scmmsg);
  exit (5); /* should never get here */
}

static void chicken_throw_exception(C_word value, C_word loc) C_noret;
static void chicken_throw_exception(C_word value, C_word loc)
{
  char *aborthook = C_text("\003syserror-hook");

  C_word *a = C_alloc(C_SIZEOF_STRING(strlen(aborthook)));
  C_word abort = C_intern2(&a, aborthook);

  abort = C_block_item(abort, 0);
  if (C_immediatep(abort))
    chicken_panic(C_text("`##sys#error-hook' is not defined"));

#if defined(C_BINARY_VERSION) && (C_BINARY_VERSION >= 8)
  C_word rval[4] = { abort, C_SCHEME_UNDEFINED, value, loc };
  C_do_apply(4, rval);
#else
  C_save(value);
  C_do_apply(1, abort, C_SCHEME_UNDEFINED);
#endif
}


void chicken_error (char *msg, C_word obj, C_word loc) 
{
  size_t msglen;
  C_word *a;
  C_word scmmsg;
  C_word list;

  msglen = strlen (msg);
  a = C_alloc (C_SIZEOF_STRING (msglen) + C_SIZEOF_LIST(2));
  scmmsg = C_string2 (&a, (char *) msg);
  list = C_list(&a, 2, scmmsg, obj);
  chicken_throw_exception(list, loc);
}

/* Adapted from the Ocaml function convert_flag_list: */

int convert_flag_list (C_word list, int *flags)
{
   C_word l; int res;

  res = 0;
  for (l = list; !(C_truep(C_i_null_list_p(l))); l = C_u_i_cdr(l)) 
  {
    res |= flags[C_unfix(C_u_i_car(l))];
  }

  return res;
}


#ifdef HAVE_POSIX_SHM
#include <sys/mman.h>
#include <sys/stat.h>        /* For mode constants */
#include <fcntl.h>           /* For O_* constants */
#endif


/**********************************************************************/
/* POSIX shared memory                                                */
/**********************************************************************/

/* This is from the POSIX realtime extensions. Not every POSIX-type OS
 * supports it.
 */

static C_word have_posix_shm =
#ifdef HAVE_POSIX_SHM
    C_SCHEME_TRUE;
#else
    C_SCHEME_FALSE;
#endif


#ifdef HAVE_POSIX_SHM
static int shm_open_flag_table[] = {
    O_RDONLY, O_RDWR, O_CREAT, O_EXCL, O_TRUNC
};
#endif


C_word chicken_shm_open(char *path, int flags, int perm)
{
    C_word result;
#ifdef HAVE_POSIX_SHM
    int ret;

    ret = shm_open(path, flags, perm);

    if (ret == -1) 
    {
      C_word *p; size_t pathlen;
      pathlen = strlen(path);
      p = C_alloc (C_SIZEOF_STRING (pathlen));
      C_word *l; size_t loclen;
      char *loc = "shm_open";
      loclen = strlen(loc);
      l = C_alloc (C_SIZEOF_STRING (loclen));
      chicken_error ("Unable to shm_open", C_string(&p, pathlen, path), C_string(&l, loclen, loc));
    } else
      {
         result = C_fix(ret);
      }
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);
}


C_word chicken_shm_unlink(char *path)
{
    C_word result;
#ifdef HAVE_POSIX_SHM
    int ret;

    ret = shm_unlink(path);

    if (ret == -1) 
    {
         C_word *p; size_t pathlen;
         pathlen = strlen(path);
         p = C_alloc (C_SIZEOF_STRING (pathlen));
         C_word *l; size_t loclen;
         char *loc = "shm_unlink";
         loclen = strlen(loc);
         l = C_alloc (C_SIZEOF_STRING (loclen));
         chicken_error ("Unable to shm_open", C_string(&p, pathlen, path), C_string(&l, loclen, loc));
    } else
      {
         result = C_fix(ret);
      }
    result = C_fix(ret);
#else
    result = C_SCHEME_FALSE;
#endif
    C_return(result);

}

<#

(define posix-shm? (foreign-value  "have_posix_shm"  bool))

(define chicken_shm_open (foreign-safe-lambda scheme-object "chicken_shm_open" nonnull-c-string int int))

(define valid-flags (list open/rdonly open/rdwr open/creat open/excl open/trunc))

(define (shm-open path oflags #!key (mode (+ perm/irwxu perm/irgrp perm/iroth)))
  (let ((oflags1 (apply + (filter (lambda (x) (member x valid-flags)) oflags))))
    (chicken_shm_open path oflags1 mode)))

(define shm-unlink (foreign-safe-lambda scheme-object "chicken_shm_unlink" nonnull-c-string))



)
