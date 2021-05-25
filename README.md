# FFTPACK v1.0
FFTPack aims to provide an easily usable package of functions using FFTPack library (Fortran 77).

## Getting started
### Get the code
```bash
git clone https://github.com/certik/fftpack.git
cd fftpack
```

### Supported Compilers
The following combinations are tested on the default branch of stdlib:  
|Name|Vesrion|Platform|Architecture|  
|---|---|---|---|  
|GCC Fortran(MSYS2)|10|Windows 10|x86_64|  

### Build with Make
You can build using provided Makefiles:
```bash
make
make test
```

### Build with fpm
You can build using provided `fpm.toml`:
```bash
fpm build --flag -Wall
fpm test --flag -Wall
```
To use fftpack within your fpm project, add the following to fpm.toml file:
```toml
[dependencies]
fftpack = {git = "https://github.com/certik/fftpack.git" }
```

## Links
[netlib/fftpack1.0](http://www.netlib.org/fftpack/)