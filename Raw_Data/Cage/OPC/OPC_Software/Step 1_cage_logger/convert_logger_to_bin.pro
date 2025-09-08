PRO convert_logger_to_bin, file

files = findfile('*.D00', count=nfiles)
if (nfiles gt 0) then begin
	opcfile = {file: '', cast: 0, date_str: '', count: 0L}
	opcfile = replicate(opcfile, nfiles)
	for i = 0, nfiles - 1 do begin
		read_focal_opc, files(i), id, data, header=header
		opcfile(i).file = files(i)
		opcfile(i).cast = fix(strmid(files(i), 3, 3))
		opcfile(i).date_str = strmid(string(header), 5, 17)
		opcfile(i).count = n_elements(id)
		print, opcfile(i).file, opcfile(i).count
		end
	end

header_remainder = bytarr(6)
footer_remainder = bytarr(6)
word = bytarr(2)

f = 0
rec_struct = {start_date_str: '', end_date_str: '', pos: 0L, count: 0L, assigned_cast: -1}

openr, lu, file, /get_lun

flag = 0
count = 0L

done = 0
repeat begin
	if (flag eq 1) then begin
		point_lun, -lu, pos
		rec(f).pos = pos
		flag = 0
		count = 0L
		end

	readu, lu, word

	if (word(0) eq '0F'X and word(1) eq 'D8'X) then begin
		readu, lu, header_remainder
		header = [word, header_remainder]
		year = header(2)
		month = header(3)
		day = header(4)
		hour = header(5)
		minute = header(6)
		second = header(7)
		start_date_str = string(format='(I2.2,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",I2.2)', month, day, year, hour, minute, second)

		if (f eq 0) then $
			rec = [rec_struct] $
		else $
			rec = [rec, rec_struct]
		rec(f).start_date_str = start_date_str
		flag = 1
		end

	if (word(0) eq 'F0'X and word(1) eq 'BE'X) then begin
		readu, lu, footer_remainder
		footer = [word, footer_remainder]
		year = footer(2)
		month = footer(3)
		day = footer(4)
		hour = footer(5)
		minute = footer(6)
		second = footer(7)
		end_date_str = string(format='(I2.2,"/",I2.2,"/",I2.2," ",I2.2,":",I2.2,":",I2.2)', month, day, year, hour, minute, second)
		rec(f).end_date_str = end_date_str
		rec(f).count = count
		if (nfiles gt 0) then begin
			j = where(opcfile(*).date_str eq rec(f).start_date_str and opcfile(*).count eq count, jcount)
			print, file, count, jcount
			if (jcount eq 1) then $
				rec(f).assigned_cast = opcfile(j(0)).cast
			end
		f = f + 1
		end

	count = count + 1L

	if (eof(lu) eq 1) then $
		done = 1

	end until (done eq 1)

close, lu
free_lun, lu

nfiles = f

done = 0
last_selection = -1
first = 1

repeat begin
	if (first eq 1) then begin
		for f = 0, nfiles - 1 do begin
			text = ''
			if (rec(f).assigned_cast ne -1) then $
				text = ', cast ' + strtrim(rec(f).assigned_cast, 2)
			print, string(format='(I3)', f+1), '.  ', $
			rec(f).start_date_str, ' - ', rec(f).end_date_str, ', ', strtrim(rec(f).count, 2), ' records', text
			end

		selection = 0
		read, selection, prompt='Choose file to convert (-1 to end) -> '
		first = 0
		end $
	else $
		selection = last_selection + 1

    if (selection lt 1 or selection gt nfiles) then $
		done = 1 $
	else begin
		f = selection - 1
		last_selection = selection
		print
		print, 'File ', strtrim(selection, 2), ': ', rec(f).start_date_str, '  -  ', rec(f).end_date_str, $
			',  ', strtrim(rec(f).count, 2), ' records'
		print

		cast = 0
		read, cast, prompt='Assign what cast number to file ' + strtrim(selection, 2) + ' (0 for none, -1 to end) -> '
		if (cast lt 0) then $
			done = 1 $
		else begin
			if (cast gt 0) then begin
				filename = 'OPC' + string(format='(I3.3)', cast) + '.D00'
				print
				print, 'Writing file ', strtrim(selection, 2), ' to ', filename
				print
				rec(f).assigned_cast = cast
				openr, lu, file, /get_lun
				openw, lu_out, filename, /get_lun
				point_lun, lu, rec(f).pos
;				data = bytarr(2L * rec(f).count)
;				readu, lu, data
				header = bytarr(27)
				header(0:4) = byte('WHOI ')
				header(5:21) = byte(rec(f).start_date_str)
				header(22:23) = 32
				header(24:26) = [10, 10, 13]
				writeu, lu_out, header
				word = bytarr(2)
				print, rec(f).count
				for i = 0L, rec(f).count - 1 do begin
					readu, lu, word
					writeu, lu_out, word
					end
				close, lu, lu_out
				free_lun, lu, lu_out
				end
			end
		end
	end until (done eq 1)

END
