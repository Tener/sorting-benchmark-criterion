#!/bin/sh

mkdir -p pdf/timing
mkdir -p png/timing
mkdir -p csv/timing
mkdir -p svg/timing

mkdir -p pdf/kernel
mkdir -p png/kernel
mkdir -p csv/kernel
mkdir -p svg/kernel

mv -f *timings*.pdf pdf/timing 
mv -f *timings*.png png/timing 
mv -f *timings*.csv csv/timing 
mv -f *timings*.svg svg/timing 

mv -f *densities*.pdf pdf/kernel 
mv -f *densities*.png png/kernel 
mv -f *densities*.csv csv/kernel 
mv -f *densities*.svg svg/kernel 
