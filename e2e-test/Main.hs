module Main where

import Coalmine.Prelude
import qualified Pgenie.Client as Client
import qualified Pgenie.Protocol as Protocol

main =
  let op =
        Client.process
          (Protocol.Version 1 0 0)
          1
          config
          ( fromList
              [ ( "1.sql",
                  [i|
                    
                    create table "genre" (
                      "id" int4 not null generated always as identity primary key,
                      "name" text not null unique
                    );

                    create table "artist" (
                      "id" int4 not null generated always as identity primary key,
                      "name" text not null unique
                    );

                    create table "album" (
                      "id" int4 not null generated always as identity primary key,
                      -- Album name.
                      "name" text not null unique,
                      -- The date the album was first released.
                      "released" date null
                    );

                    create table "album_genre" (
                      "album" int4 not null references "album",
                      "genre" int4 not null references "genre"
                    );

                    create table "album_artist" (
                      "album" int4 not null references "album",
                      "artist" int4 not null references "artist",
                      -- Whether it is the primary artist
                      "primary" bool not null,
                      primary key ("album", "artist")
                    );

                  |]
                ),
                ( "2.sql",
                  [i|
                    -- In this migration we're changing the type of the album "id" column
                    -- from "int4" to "int8".
                    -- Since this column is referenced from other tables, we also update them.


                    alter table album
                    alter column id type int8;

                    alter table album_genre
                    alter column album type int8;

                    alter table album_artist
                    alter column album type int8;

                  |]
                )
              ]
          )
          ( fromList
              [ ( "insert-album.sql",
                  [i|
                    insert into album (name, released)
                    values ($$name, $$released)
                    returning id
                  |]
                ),
                ( "select-album-by-artist.sql",
                  [i|
                    select album.*
                    from album
                    left join album_artist on album_artist.album = album.id
                    where artist = $$artist
                  |]
                ),
                ( "select-genre-by-artist.sql",
                  [i|
                    select id, genre.name
                    from genre
                    left join album_genre on album_genre.genre = genre.id
                    left join album_artist on album_artist.album = album_genre.album
                    where album_artist.artist = $$artist
                  |]
                )
              ]
          )
      config =
        [i|
          space: my-space
          name: music-catalogue
          version: 1.0.0
        |]
   in do
        res <- Client.runOpHappily op True "api.pgenie.io" Nothing
        putStrLn "Success"
