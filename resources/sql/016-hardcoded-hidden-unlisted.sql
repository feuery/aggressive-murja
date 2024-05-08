ALTER TABLE blog.Post
ADD COLUMN IF NOT EXISTS hidden BOOLEAN NOT NULL DEFAULT TRUE,
ADD COLUMN IF NOT EXISTS unlisted BOOLEAN NOT NULL DEFAULT FALSE;

UPDATE blog.Post SET hidden = tags ? 'hidden';
UPDATE blog.Post SET unlisted = tags ? 'unlisted';
update blog.Post set tags = tags - 'hidden';
update blog.Post set tags = tags - 'unlisted';

ALTER TABLE blog.Post_History
ADD COLUMN IF NOT EXISTS hidden BOOLEAN NOT NULL DEFAULT TRUE,
ADD COLUMN IF NOT EXISTS unlisted BOOLEAN NOT NULL DEFAULT FALSE;

UPDATE blog.Post_History SET hidden = tags ? 'hidden';
UPDATE blog.Post_History SET unlisted = tags ? 'unlisted';
update blog.Post_History set tags = tags - 'hidden';
update blog.Post_History set tags = tags - 'unlisted';

CREATE OR REPLACE FUNCTION push_to_history()
RETURNS TRIGGER
LANGUAGE plpgsql
AS
$$
DECLARE local_version INT;
BEGIN
	SELECT  coalesce(MAX(ph.version), 0) + 1 INTO local_version 
	FROM blog.Post p
	LEFT JOIN blog.Post_History ph ON p.ID = ph.ID
	WHERE p.id = OLD.id
	GROUP BY p.ID;

	INSERT INTO blog.Post_History(ID, Title, Content, creator_id, tags, created_at, version, hidden, unlisted)
	VALUES (OLD.ID, OLD.Title, OLD.Content, OLD.creator_id, OLD.tags, OLD.created_at, local_version, OLD.hidden, OLD.unlisted);

	RETURN NEW;
END;
$$;

