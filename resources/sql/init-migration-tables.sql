CREATE TABLE IF NOT EXISTS public.migrations_tracker
(
	id varchar(255) NOT NULL PRIMARY KEY,
	created_at timestamp NOT NULL
);

CREATE TABLE IF NOT EXISTS public.ragtime_migrations
(
	id varchar(255) NOT NULL PRIMARY KEY,
	created_at varchar(32) NOT NULL
);

INSERT INTO public.migrations_tracker
SELECT id, created_at::timestamp
FROM public.ragtime_migrations
WHERE id NOT IN (SELECT id FROM public.migrations_tracker);
