# renaming-hell
A hellish conception of a simulated system using register renaming

This doesn't actually work right yet since I don't have a system in place to decide when a register is safe
to reallocate (this will need some kind of writeback queue).

Anyway the idea is already there, now I just need to read papers to actually make it work for long enough dependency chains that we
run out of rename target registers (since even with stalls it will never free any and we run out after 31 renames)

Nya there's lots of ideas here
