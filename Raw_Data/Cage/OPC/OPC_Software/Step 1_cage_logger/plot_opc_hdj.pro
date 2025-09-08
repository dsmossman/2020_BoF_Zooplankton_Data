PRO plot_opc, cast, cf5=cf5, yrange1=yrange1, yrange2=yrange2, reset=reset, $
              lopc_file=lopc_file, vdist_only=vdist_only, maxd=maxd, maxc=maxc

if (!d.window ge 0) then $
  wshow, !d.window $
else $
  window, 0, xsize=576, ysize=512
loadct, 0, /silent
tek_color

; Change max_size to maximum particle size to plot
max_size = 4.0		; mm

if (keyword_set(lopc_file) eq 1) then begin
  lopc_hist = fltarr(256)
  openr, lu, lopc_file, /get_lun
  readf, lu, lopc_hist
  close, lu
  free_lun, lu
  lopc_x = findgen(256) * 0.015 + 0.015 / 2.0
  end

file = string(format='("pl_opc",I3.3,".bin")', cast)
f = findfile(file, count=count)
if (count eq 0 or keyword_set(reset) eq 1) then begin
  convert_single_opc, cast
  select_extents, cast
  end

color = 2

read_opc_binary, cast, rec, esd

minsize = 1.5
maxsize = 2.0

area = 0.02 * 0.25    ; area of tunnel aperture

dbin = 0.05
maxv = 5.0
nbins = floor(maxv / dbin) + 1
sbin = findgen(nbins) * dbin
ebin = sbin + dbin

hist_binned = lonarr(nbins)
abundance_binned = fltarr(nbins)

volume = 0.0
for u = 0L, n_elements(rec) - 2 do begin
  if (rec(u).flag eq 1) then begin
    dz = rec(u+1).depth - rec(u).depth
    volume = volume + area * dz
    if (rec(u).count gt 0) then $
      for v = rec(u).start, rec(u).start + rec(u).count - 1 do begin
        w = long((esd(v) - min(sbin)) / dbin)
        if (w ge 0 and w lt nbins) then $
          hist_binned(w) = hist_binned(w) + 1L
        end
    end
  end

j = where(rec(*).depth lt 1.0e30 and rec(*).depth gt 0.0)
volume2 = (max(rec(j).depth) - min(rec(j).depth)) * area
abundance_binned(*) = hist_binned(*) / volume2

if (keyword_set(vdist_only) eq 1) then $
  goto, depth_only_label

if (keyword_set(yrange1) eq 0) then $
  yrange1 = [0, 1000]
if (keyword_set(yrange2) eq 0) then $
  yrange2 = [0, 50]

plot, [0, 0], [0, 0], /nodata, xrange=[0.0, max_size], $
      yrange=yrange1, xticklen=-0.02, yticklen=-0.012, $
      ytitle='Abundance (m!E-3!N)', ymargin=[6, 4], $
      xtitle='Equivalent Circular Diameter (mm)', charsize=10.0/12.0
for u = 0, nbins - 1 do begin
  polyfill, [sbin(u), sbin(u), ebin(u), ebin(u), sbin(u)], $
            [0.0, abundance_binned(u), abundance_binned(u), 0.0, 0.0], $
            noclip=0, color=color
  if (u eq 0) then $
    plots, [sbin(u), ebin(u)], [abundance_binned(u), abundance_binned(u)], noclip=0 $
  else $
    plots, [ebin(u-1), sbin(u), ebin(u)], [abundance_binned(u-1), abundance_binned(u), abundance_binned(u)], noclip=0
  end
plots, [!x.window(0), !x.window(0), !x.window(1), !x.window(1), !x.window(0)], $
       [!y.window(0), !y.window(1), !y.window(1), !y.window(0), !y.window(0)], /normal

if (keyword_set(lopc_file) eq 1) then $
  oplot, lopc_x, lopc_hist, color=7

x0 = !x.window(0) + (!x.window(1) - !x.window(0)) * 0.65
y0 = !y.window(0) + (!y.window(1) - !y.window(0)) * 0.90
xyouts, x0, y0, 'Cast ' + strtrim(cast, 2), /normal

p = !p
x = !x
y = !y

x0 = !x.window(0) + (!x.window(1) - !x.window(0)) * 0.45
x1 = !x.window(0) + (!x.window(1) - !x.window(0)) * 0.95
y0 = !y.window(0) + (!y.window(1) - !y.window(0)) * 0.25
y1 = !y.window(0) + (!y.window(1) - !y.window(0)) * 0.80
pos = [x0, y0, x1, y1]
plot, [0, 0], [0, 0], /nodata, xrange=[0.0, max_size], $
      yrange=yrange2, xticklen=-0.02, yticklen=-0.012, $
      position=pos, charsize=8.0/12.0, /noerase
for u = 0, nbins - 1 do begin
  polyfill, [sbin(u), sbin(u), ebin(u), ebin(u), sbin(u)], $
            [0.0, abundance_binned(u), abundance_binned(u), 0.0, 0.0], $
            noclip=0, color=color
  if (u eq 0) then $
    plots, [sbin(u), ebin(u)], [abundance_binned(u), abundance_binned(u)], noclip=0 $
  else $
    plots, [ebin(u-1), sbin(u), ebin(u)], [abundance_binned(u-1), abundance_binned(u), abundance_binned(u)], noclip=0
  end

if (keyword_set(lopc_file) eq 1) then $
  oplot, lopc_x, lopc_hist, color=7

plots, [!x.window(0), !x.window(0), !x.window(1), !x.window(1), !x.window(0)], $
       [!y.window(0), !y.window(1), !y.window(1), !y.window(0), !y.window(0)], /normal

!p = p
!x = x
!y = y


;****************************************************************

print, 'Hit a key to continue or hit (S) to select a size range...'
option = strupcase(get_kbrd(1))
if (option eq 'S') then begin
  print, 'Select size range with cursor...'
  cursor, x0, y0, /data, /down
  cursor, x1, y1, /data, /down
  if (x0 gt x1) then swap, x0, x1
  minsize = x0
  maxsize = x1
  end
if (option eq 'Q') then $
  goto, theend

;****************************************************************

depth_only_label:

print, 'Size range: ', minsize, maxsize

maxz = 350.0
dz = 5.0
nbins = floor(maxz / dz) + 1
sbin = findgen(nbins) * dz
ebin = sbin + dz
bin = fltarr(nbins)
time = 0.0
bin_flag = bytarr(nbins)

for i = 0L, n_elements(rec) - 1 do begin
  if (rec(i).depth gt 0.0 and rec(i).depth lt maxz and $
      rec(i).count gt 0 and rec(i).flag eq 1) then begin
    p = floor(rec(i).depth / dz)
    j0 = rec(i).start
    time = time + 0.5
    bin_flag(p) = 1B
    for j = 0, rec(i).count - 1 do $
      if (esd(j0+j) ge minsize and esd(j0+j) le maxsize) then $
        bin(p) = bin(p) + 1
    end
  end

max_depth = maxz
j = where(rec(*).depth lt 1.0e30, jcount)
if (jcount gt 0) then $
  max_depth = max(rec(j).depth)

if (keyword_set(maxd) eq 1) then $
  max_depth = maxd

conc = bin(*) / (area * dz)   ; Convert counts to counts per cubic meter

xtitle = 'Concentration (particles m!E-3!N)'
if (keyword_set(cf5) eq 1 and minsize eq 1.5 and maxsize eq 2.0) then begin
  coeff0 = 0.038414654
  coeff1 = 0.53425482
  coeff2 = 0.80007076
  speed = total(bin_flag) * dz / time
  print, 'Descent speed:  ', strtrim(speed, 2), ' m/s'
  j = where(conc gt 0.0, jcount)
  if (jcount gt 0) then begin
    lopc = alog10(conc(j))
    lnet = (lopc - coeff0 - coeff2 * speed) / coeff1
    conc(*) = 0.0
    conc(j) = 10.0 ^ lnet
    end
  xtitle = 'Calanus finmarchicus C5 Abundance (copepods m!E-3!N)'
  end

if (keyword_set(maxc) eq 0) then $
  maxc = max(conc)

plot, [0, 0], [0, 0], xrange=[0, maxc], yrange=[max_depth, 0], /nodata, $
      xtitle=xtitle, ytitle='Depth (m)', ticklen=-0.02, ymargin=[6, 4]
for i = 0, nbins - 1 do $
  polyfill, [0, conc(i), conc(i), 0], [sbin(i), sbin(i), ebin(i), ebin(i)], color=color, noclip=0
plots, !x.crange, [1, 1] * max_depth, linestyle=1
plots, [!x.window(0), !x.window(0), !x.window(1), !x.window(1), !x.window(0)], $
       [!y.window(0), !y.window(1), !y.window(1), !y.window(0), !y.window(0)], /normal

;for i = 0, nbins - 1 do $
;  print, sbin(i), ebin(i), conc(i)

;***START HDJ addition***

;define file name
file=string(format='("OPC",I3.3,".csv")',cast)

;write binned concentrations to csv file
write_csv,file,sbin,ebin,conc

;***END HDJ addition***

theend:
END
