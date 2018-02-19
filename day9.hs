-- in perl:
-- part 1:
-- perl -pe 's/!.//g;s/<.*?>//g' day9.input.txt | perl -ne '$s=$d=0;while(m/\{|\}/g){$&eq"{"?$s+=++$d:$d--}print "$s\n"'

-- part 2:
-- perl -pe 's/!.//g' day9.input.txt | perl -ne '$s=0;while(m/<.*?>/g){$s+=(length $&)-2}print $s'