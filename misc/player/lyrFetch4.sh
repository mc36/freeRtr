#!/usr/bin/perl
# cpan
# cpan -u
# cpan -Ti Lyrics::Fetcher
# cpan -Ti Lyrics::Fetcher::AZLyrics
# cpan -Ti Lyrics::Fetcher::LyrDB
# cpan -Ti Lyrics::Fetcher::AstraWeb
# cpan -Ti Lyrics::Fetcher::Lyrics::Fetcher::LyricsDownload
# cpan -Ti Lyrics::Fetcher::LyricWiki
use Lyrics::Fetcher;
print Lyrics::Fetcher->fetch($ARGV[0],$ARGV[1],'LyricWiki');
