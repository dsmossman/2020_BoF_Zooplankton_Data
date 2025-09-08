PRO read_focal_opc, file, id, data, header=header

; Reads raw OPC data file wriiten with Focal Technologies'
; Data Acquisition Software package

openr, lu, file, /get_lun

;p = strpos(file, '.d00')	; if .d00 is file extension, it has a new file
;q = strpos(file, '.D00')	; format with 27 instead of 23 header bytes
;if (p lt 0 and q lt 0) then $
;  nheader = 23 $
;else $
;  nheader = 27

nheader = 27

header = bytarr(nheader)

fs = fstat(lu)
n = (fs.size - nheader) / 2
if (n gt 32768) then $			; limit for IDL Student version
  n = 32768L
words = bytarr(2, n)

readu, lu, header
readu, lu, words

close, lu
free_lun, lu

id = reform((words(0,*) and 'F0'XB) / '10'XB)
data = reform(fix(words(0,*) and '0F'XB) * '100'X + fix(words(1,*)))

i = 0L
flag = replicate(1B, n)
while (i lt n) do begin
  if (id(i) eq 11 or id(i) eq 14) then begin
    case id(i) of
      11: skip = 4
      14: skip = 13
      end
    j = (i + skip - 1) < (n - 1)
    flag(i:j) = 0B
    i = j
    end
  i = i + 1
  end

j = where(flag eq 1B, jcount)
if (jcount gt 0) then begin
  id = id(j)
  data = data(j)
  end

END
