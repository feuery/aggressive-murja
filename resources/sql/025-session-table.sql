CREATE TABLE blog.serialized_session
(
  session_key uuid not null default gen_random_uuid () primary key,
  owner int not null,
  expires_at timestamp NOT NULL DEFAULT NOW() + '3 months',
  foreign key (owner) references blog.users(id)
    on update cascade
    on delete cascade
);

CREATE TABLE blog.session_store
(
  session_key uuid not null, 
  var_name text not null,
  val text not null,
  primary key (session_key, var_name),
  foreign key (session_key) references blog.serialized_session(session_key)
    on update cascade
    on delete cascade  
);    

-- related to session cookies 
INSERT INTO blog.Settings VALUES ('domain', '"http://example.com"')
ON CONFLICT DO NOTHING;
