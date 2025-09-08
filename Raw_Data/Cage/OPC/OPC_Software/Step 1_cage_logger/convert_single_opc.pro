FUNCTION convert_digital_size, ds_in

ds = float(ds_in)

a0 = 10879.8
a1 = 1.923
s = sqrt(ds)
term = a0 / ((exp(abs(3.65 - (ds / 1000.0)))) ^ a1)
esd = (2088.76 + term + 85.85 * s) * (1 - exp(-0.0465 * s + 0.00008629 * ds))
esd = esd / 1000.0

return, esd

END


PRO convert_single_opc, cast, file=file, outfile=outfile

if (keyword_set(file) eq 0) then $
  file = string(format='("OPC",I3.3,".D00")', cast)
if (keyword_set(outfile) eq 0) then $
  outfile = string(format='("pl_opc",I3.3,".bin")', cast)

print, 'Converting ', file, ' -> ', outfile

read_focal_opc, file, id, data

fill = 9.9692099683868690e+36

j = where(id eq 1, jcount)
esd = fltarr(jcount)

rec = {timer: 0, atten: 0, depth: 0.0, flow: 0.0, flag: 0B, start: 0L, count: 0L}
j = where(id eq 3, jcount)
rec = replicate(rec, jcount)

; Calibration coefficients
; For OSU/Mate OPC
depth_m = 125.0
depth_b = -125.0
depth_sp_grav = 1.027
; For UMass/Zhou OPC
;depth_m = 123.0
;depth_b = 0.0
;depth_sp_grav = 1.0
flow_m = 0.130
flow_b = 0.0370
; For WHOI OPC
depth_m = 250.0
depth_b = -250.0

first_timer = 0
atten = 4095
depth = fill
flow = fill

for i = 0L, n_elements(id) - 1 do begin
  case id(i) of
       1: begin         ; Particle record
            if (first_timer eq 1) then begin
            esd(k) = convert_digital_size(data(i))
              if (count eq 0) then $
                rec(j).start = k
              k = k + 1
              count = count + 1
              end
            end
       2: begin         ; Light attenuance record
            atten = data(i)
            end
       3: begin         ; Time record
            if (first_timer eq 0) then begin
             j = 0L
              k = 0L
              first_timer = 1
              end $
            else begin
              if (count eq 0) then $
                rec(j).start = -1L
              rec(j).count = count
              j = j + 1
              end
            count = 0L
            rec(j).timer = data(i)
            rec(j).atten = atten
            rec(j).depth = depth
            rec(j).flow = flow
            atten = 4095
            depth = fill
            end
       5: begin         ; Depth record
            x = 5.0 * float(data(i)) / 4095.0
            p = depth_m * x + depth_b
            depth = p * 0.70307 / depth_sp_grav
            end
       8: begin         ; Flow record
            x = flow_m * float(data(i)) + flow_b
            flow = x
            end
   else: begin
            end
    end
  end

if (count eq 0) then $
  rec(j).start = -1L
rec(j).count = count

nrec = j + 1
n = k

flag = bytarr(nrec)
j = where(rec(*).depth gt 1.0e30, jcount)
if (jcount gt 0) then $
  flag(j) = 1B

if (!version.os eq 'IRIX') then begin
  nrec = swap_endian(nrec)
  n = swap_endian(n)
  rec = swap_endian(rec)
  esd = swap_endian(esd)
  end

openw, lu, outfile, /binary, /get_lun
writeu, lu, nrec
writeu, lu, n
writeu, lu, rec
writeu, lu, esd
close, lu
free_lun, lu

END
