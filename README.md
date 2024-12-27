# Aggressive Murja 
<sup><sub>(as in "aggressiivinen viinimurjapaska)</sub></sup>

## What is this 
A blog engine written in Common Lisp (backend) and Elm (frontend, currently, although I'm toying with redoing this on something else). 

Murja supports:
- blog feed (obviously :D)
- rss-feeds 
  Murja can subscribe to rss/atom feeds and download new articles into the local database periodically (although the periodic polling must be done outside of murja by setting up a cronjob to curl /api/rss/update periodically). These articles can then be read in murja's ui. There's also support for creating your own blogposts with the "another-feed said: some excerpt from the linked article" -template in the rss reader ui.
- images 
  If you drag and drop an image into the post-editor, the image is uploaded, stored in the db and an &lt;img&gt; inserted into the article.
- Tags and previouslies 
  for hierarchical reasons.
- Hidden and unlisted posts 
  Hidden are hidden from everybody but their owner, unlisted are accessible to everybody that have their url but are supposed to be hidden in the ui and search.
  
Murja supports theoretically multiple users, but has never been tested with more than one user.

## Dependencies
Murja needs:
- postgresql (a hard dependency due to lisp missing a jdbc-like interface to every db under the sun and me using Postmodern as the db api)
- some kind of a lisp environment, murja is developed and run in feuerx.net inside sbcl but I don't know why this wouldn't work in other lisps.

## Installation
Github Actions spams new release on every commit to main into ghcr.io (image called ghcr.io/feuery/murja:latest ).

I'm running murja with these environment variables set up in the docker-compose yml:
- MURJA_DB_HOST
  postgresql host 
- MURJA_DB_ADMIN
  postgresql's username 
- MURJA_DB
  db where murja stores its contents 
- MURJA_DB_PASSWD
  db password :D 
  
The initial introduction process is _really_ bad, due to me not needing it as I've just been restoring a db dump that's been accumulating since 2016. Improving it is on the backlog.

To start using murja, you need to insert a row into blog.users (password is SHA-512). There is a convenience function murja.users.user-db:register-user, but I don't know if it's possible to get a repl inside a production murja image. 

After that, you need to link your user to admin-group by running 
```sql 
INSERT INTO blog.groupmapping
SELECT usr.id, grp.id, true
FROM blog.users usr
JOIN blog.usergroup grp ON grp.name = 'Admins'
ON CONFLICT DO NOTHING;
```

Then the app should be usable in port http://localhost:3010 or wherever you're running the app server.
