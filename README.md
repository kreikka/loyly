loyly
=====

Web pages for Helsingin Akateemiset Löylyttelijät ry.

file structure
--------------

Important paths:

* templates/: most of the page contents
* config/routes: available pages
* config/models: database models
* Handler/: page logic

blog file format
----------------

    ---
    author: Teh Author
    title: An example post
    date: 2014-10-16T11:43:29+0300
    tags: one tag, another tag, a third tag
    ---

Accounts
--------

For accounts we use the package `yesod-auth-account`.

These might be necessary when upgrading:

    drop table "user";
    update member set member_since = '2014-11-09 00:00:00';
