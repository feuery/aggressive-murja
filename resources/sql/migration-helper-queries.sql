-- name: migration-exists
-- returns: :array-hash

SELECT COUNT(*) as "count"	
FROM public.migrations_tracker
WHERE id = $1;

-- name: migration-table-exists*
-- returns: :array-hash

SELECT EXISTS (
   SELECT FROM information_schema.tables 
   WHERE  table_schema = 'public'
   AND    table_name   = 'migrations_tracker');

-- name: mark-migration-done @execute
INSERT INTO public.migrations_tracker VALUES ($1, NOW());
