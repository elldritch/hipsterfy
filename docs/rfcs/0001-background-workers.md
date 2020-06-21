# Background workers

## Overview

Right now, we load 6 sets of artists whenever a user makes a `POST` to `/compare` with a friend code.

For both the user and their friend, we load:

1. Their followed artists.
2. The artists of their saved tracks.
3. The artists of their saved albums.

We then load the artist insights for each of these artists. This takes a really long time for something on a live user path. For me, this set of followed artists numbers about 5k, and this request is slow enough that it triggers an NGINX gateway timeout when deployed to production.

Instead of loading these artists on the request path, we should load them in the background.

## Design

There are two paths here that can be optimized:

1. Given a user, we need to know what artists they follow.
2. Given an artist, we need to know how many monthly listeners they have.

Right now, these both occur in the `/compare` request path. Instead, we'll move as much of this to the background as possible.

### Keeping artist information up-to-date

Conceptually, an artist is composed of:

- Spotify ID
- Name
- Spotify URL
- Monthly listener count

We'll create a new job called `updateArtist` that takes a Spotify ID as a parameter, checks if that artist is up-to-date in the database, and updates the artist if not up-to-date or inserts the artist if it does not exist.

Multiple `updateArtist` jobs may be enqueued for the same artist. `updateArtist` jobs should be cheap (check then no-op) for up-to-date artists.

An artist is up-to-date if all of its fields are present in its database row, and the monthly listener count for this month has been added.

We'll build two new long-running workers:

1. The `updateArtistWorker` completes `updateArtist` jobs by querying the Spotify API.
2. The `monthlyUpdateWorker` periodically examines all artist database rows and enqueues new `updateArtist` jobs whenever it sees an artist with an outdated monthly listener count.

### Loading the artists of a user

We'll add a join table to save which users follow which artists.

We'll create a new job called `updateUser` which takes a user ID as a parameter and updates the user's followed artists, enqueuing `updateArtist` jobs for all artists encountered.

Only one `updateArtist` job may be enqueued or running for a single user. This invariant is enforced at insertion time.

Whenever a user signs into the home page we'll record the sign-in timestamp. If a user has not signed in in at least a week, we'll enqueue an `updateUser` job.

We'll also display the user's followed artists on their home page, and add a "refresh followed artists" button that refreshes the user's followed artists.

### Reducing request delays with JavaScript

Users may use the `/compare` route before information on their followed artists has completely loaded. In that case, we'll record the partial progress made on loading their information, and we'll provide JavaScript that progressively enhances clients by automatically refreshing the table view when new information is available.

We'll need to save this progress information as part of the `updateUser` and `updateArtist` jobs.

For `updateUser`, we'll need to know the total number of followed artists, and the number of followed artists currently known. (This is because we need to paginate sequentially through all followed artists.)

To do this, we'll need to save "total followed artist" information as an extra step in the beginning (we'll need to query the API for the totals of each type of followed artist, and even this will be an estimate for album and track artists) and we'll need to save how many pages we've already loaded. This information will be saved on the job itself.

For `updateArtist`, we'll need to know which artists have unavailable or stale monthly listener counts. We'll know this by the state of the monthly listener database row, so we don't need to save any extra information.
