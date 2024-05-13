CREATE TABLE IF NOT EXISTS blog.Settings
(
	key TEXT NOT NULL PRIMARY KEY,
	value JSONB NOT NULL
);

INSERT INTO blog.Settings VALUES ('time-format', '"dd.MM.yyyy HH:mm"'),
                                 ('blog-title', '"Murja.dev @ $HOSTNAME"'),
                                 ('recent-post-count', '6')
ON CONFLICT DO NOTHING;
--for reasons unknown SERIAL is broken
-- but then, the id is supposed to be stable, so this should be fine
INSERT INTO blog.Permission (id, action) VALUES (13, 'update-settings') ON CONFLICT DO NOTHING;

INSERT INTO blog.GroupPermissions VALUES ((select id from blog.Permission where action = 'update-settings')
       	    			  	 , (select id from blog.UserGroup where name = 'Admins')) ON CONFLICT DO NOTHING;
