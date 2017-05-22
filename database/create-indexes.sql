-- This is required to speed up query time.
create unique index if not exists wunderlist_id_index on relation (wunderlist_id asc);
