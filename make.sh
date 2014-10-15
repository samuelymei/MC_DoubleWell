FC=gfortran
FFLAGS=""
${FC} ${FFLAGS} -c fp_kind.f90
${FC} ${FFLAGS} -c lib.f90
${FC} ${FFLAGS} -c MC_DoubleWell.f90
${FC} ${FFLAGS} -c MC_run.f90
${FC} ${FFLAGS} fp_kind.o lib.o MC_DoubleWell.o MC_run.o -o MC_DoubleWell.x
rm *.o *.mod
