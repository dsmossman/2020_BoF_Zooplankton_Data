PRO select_extents, cast, reset=reset

read_opc_binary, cast, rec, esd

nrec = n_elements(rec)
n = n_elements(esd)

if (keyword_set(reset) eq 1) then begin
  rec(*).flag = 0B
  end $
else begin
  if (!d.window ge 0) then $
    wshow, !d.window
  plot, rec(*).depth, max_value=1.0e30, yrange=[300, 0]
  print, 'Choose start and end of profile with cursor...'
  cursor, x0, y0, /data, /down
  cursor, x1, y1, /data, /down
  if (x0 gt x1) then swap, x0, x1
  i0 = ceil(x0)
  i1 = floor(x1)
  if (i0 lt 0) then $
    i0 = 0L
  if (i1 ge nrec) then $
    i1 = nrec - 1
  rec(*).flag = 0B
  rec(i0:i1).flag = 1B
  end

if (!version.os eq 'IRIX') then begin
  nrec = swap_endian(nrec)
  n = swap_endian(n)
  rec = swap_endian(rec)
  esd = swap_endian(esd)
  end

outfile = string(format='("pl_opc",I3.3,".bin")', cast)
openw, lu, outfile, /binary, /get_lun
writeu, lu, nrec
writeu, lu, n
writeu, lu, rec
writeu, lu, esd
close, lu
free_lun, lu

END
