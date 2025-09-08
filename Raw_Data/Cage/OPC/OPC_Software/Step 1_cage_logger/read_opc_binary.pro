PRO read_opc_binary, cast, rec, esd, file=file

swap = 0B
if (!version.os eq 'IRIX') then swap = 1B

if (keyword_set(file) eq 0) then $
  file = string(format='("pl_opc",I3.3,".bin")', cast)

nrec = 0L
n = 0L

openr, lu, file, /binary, /get_lun

readu, lu, nrec
readu, lu, n

if (swap eq 1) then begin
  nrec = swap_endian(nrec)
  n = swap_endian(n)
  end

rec = {timer: 0, atten: 0, depth: 0.0, flow: 0.0, flag: 0B, start: 0L, count: 0L}
rec = replicate(rec, nrec)
esd = fltarr(n)

readu, lu, rec
readu, lu, esd

close, lu
free_lun, lu

if (swap eq 1) then begin
  rec = swap_endian(rec)
  esd = swap_endian(esd)
  end

END

