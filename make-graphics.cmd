setlocal

set bmp2tile="../Code/Delphi/BMP to tile/bmp2tile.exe"

%bmp2tile% coloured-tiles.png -savetiles "coloured-tiles.bin" -exit
%bmp2tile% font-small.png -noremovedupes -savetiles "font-small.1bpp" -exit
%bmp2tile% font-large-digits.png -noremovedupes -savetiles "font-large-digits.1bpp" -exit
%bmp2tile% font-large-alpha.png -noremovedupes -savetiles "font-large-alpha.1bpp" -exit
