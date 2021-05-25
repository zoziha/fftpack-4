# fpm -flag: -fcheck-bounds

```bash
fpm test
```

Default `FPM` using this flag `-fcheck-bounds` will result in an error:  
At line 5 of file.src \dffti.f
Fortran runtime error: Index '121' of Array 'wsave' above upper bound of 1.

```fortran
      SUBROUTINE DFFTI (N,WSAVE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION       WSAVE(1)
      IF (N .EQ. 1) RETURN
      CALL RFFTI1 (N,WSAVE(N+1),WSAVE(2*N+1))
      RETURN
      END
```

Override the default `FPM` flag can solve this problem for a while, like:
```bash
fpm test --flag -Wall
```