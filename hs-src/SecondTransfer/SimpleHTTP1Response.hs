{-# LANGUAGE OverloadedStrings #-}


module SecondTransfer.SimpleHTTP1Response(exampleHTTP11Response, exampleHTTP20Response, shortResponse) where

import qualified Data.ByteString as B 

-- Just to check what a browser thinks about this port 

exampleHTTP11Response :: B.ByteString
exampleHTTP11Response = "HTTP/1.1 200 OK\r\n\
\Content-Length: 1418\r\n\
\Keep-Alive: timeout=5, max=100\r\n\
\Content-Type: text/html\r\n\
\\r\n\
\<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\r\n\
\<HTML>\r\n\
\ <HEAD>\r\n\
\   <TITLE> [Haskell-cafe] multiline strings in haskell?\r\n\
\   </TITLE>\r\n\
\   <LINK REL=\"Index\" HREF=\"index.html\" >\r\n\
\   <LINK REL=\"made\" HREF=\"mailto:haskell-cafe%40haskell.org?Subject=%5BHaskell-cafe%5D%20multiline%20strings%20in%20haskell%3F&In-Reply-To=Pine.LNX.4.44.0601121646300.23112-100000%40peano.math.uni-bremen.de\">\r\n\
\   <META NAME=\"robots\" CONTENT=\"index,nofollow\">\r\n\
\   <META http-equiv=\"Content-Type\" content=\"text/html; charset=us-ascii\">\r\n\
\   <LINK REL=\"Previous\"  HREF=\"013910.html\">\r\n\
\   <LINK REL=\"Next\"  HREF=\"013901.html\">\r\n\
\ </HEAD>\r\n\
\ <BODY BGCOLOR=\"#ffffff\">\r\n\
\   <H1>[Haskell-cafe] multiline strings in haskell?</H1>\r\n\
\    <B>Sebastian Sylvan</B> \r\n\
\    <A HREF=\"mailto:haskell-cafe%40haskell.org?Subject=%5BHaskell-cafe%5D%20multiline%20strings%20in%20haskell%3F&In-Reply-To=Pine.LNX.4.44.0601121646300.23112-100000%40peano.math.uni-bremen.de\"\r\n\
\       TITLE=\"[Haskell-cafe] multiline strings in haskell?\">sebastian.sylvan at gmail.com\r\n\
\       </A><BR>\r\n\
\    <I>Thu Jan 12 11:04:43 EST 2006</I>\r\n\
\    <P><UL>\r\n\
\        <LI>Previous message: <A HREF=\"013910.html\">[Haskell-cafe] multiline strings in haskell?\r\n\
\</A></li>\r\n\
\        <LI>Next message: <A HREF=\"013901.html\">[Haskell-cafe] multiline strings in haskell?\r\n\
\</A></li>\r\n\
\         <LI> <B>Messages sorted by:</B> \r\n\
\              <a href=\"date.html#13911\">[ date ]</a>\r\n\
\              <a href=\"thread.html#13911\">[ thread ]</a>\r\n\
\              <a href=\"subject.html#13911\">[ subject ]</a>\r\n\
\              <a href=\"author.html#13911\">[ author ]</a>\r\n\
\         </LI>\r\n\
\       </UL>\r\n\
\    <HR>  \r\n\
\<!--beginarticle-->\r\n\
\<PRE>On 1/12/06, Henning Thielemann &lt;<A HREF=\"http://www.haskell.org/mailman/listinfo/haskell-cafe\">lemming at henning-thielemann.de</A>&gt; wrote:\r\n\
\&gt;<i>\r\n\
\</I>&gt;<i> On Thu, 12 Jan 2006, Jason Dagit wrote:\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> &gt; On Jan 12, 2006, at 1:34 AM, Henning Thielemann wrote:\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; &gt; On Wed, 11 Jan 2006, Michael Vanier wrote:\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt;&gt; Is there any support for multi-line string literals in Haskell?  I've\r\n\
\</I>&gt;<i> &gt; &gt;&gt; done a web search and come up empty.  I'm thinking of using\r\n\
\</I>&gt;<i> &gt; &gt;&gt; Haskell to\r\n\
\</I>&gt;<i> &gt; &gt;&gt; generate web pages and having multi-line strings would be very\r\n\
\</I>&gt;<i> &gt; &gt;&gt; useful.\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt; Do you mean\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt; unlines [&quot;first line&quot;, &quot;second line&quot;, &quot;third line&quot;]\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; The original poster probably meant something like this:\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; let foo = &quot;This is a\r\n\
\</I>&gt;<i> &gt; long string\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; Which does not end until the matching end quote.&quot;\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> I don't see the need for it, since\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> unlines [\r\n\
\</I>&gt;<i>   &quot;first line&quot;,\r\n\
\</I>&gt;<i>   &quot;second line&quot;,\r\n\
\</I>&gt;<i>   &quot;third line&quot;]\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> works as well.\r\n\
\</I>&gt;<i>\r\n\
\</I>\r\n\
\Nevertheless Haskell supports multiline strings (although it seems\r\n\
\like a lot of people don't know about it). You escape it using  and\r\n\
\then another  where the string starts again.\r\n\
\\r\n\
\str = &quot;multi \r\n\
\        \\line&quot; \r\n\
\\r\n\
\Prelude&gt;str\r\n\
\&quot;multiline&quot;\r\n\
\\r\n\
\\r\n\
\/S\r\n\
\\r\n\
\--\r\n\
\Sebastian Sylvan\r\n\
\+46(0)736-818655\r\n\
\UIN: 44640862\r\n\
\</PRE>\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\<!--endarticle-->\r\n\
\    <HR>\r\n\
\    <P><UL>\r\n\
\        <!--threads-->\r\n\

\<a href=\"http://www.haskell.org/mailman/listinfo/haskell-cafe\">More information about the Haskell-Cafe\r\n\
\mailing list</a><br>\r\n\
\</body></html>\r\n\
\\r\n"

exampleHTTP20Response :: B.ByteString
exampleHTTP20Response = "\
\<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\r\n\
\<HTML>\r\n\
\ <HEAD>\r\n\
\   <TITLE> [Haskell-cafe] multiline strings in haskell?\r\n\
\   </TITLE>\r\n\
\   <LINK REL=\"Index\" HREF=\"index.html\" >\r\n\
\   <LINK REL=\"made\" HREF=\"mailto:haskell-cafe%40haskell.org?Subject=%5BHaskell-cafe%5D%20multiline%20strings%20in%20haskell%3F&In-Reply-To=Pine.LNX.4.44.0601121646300.23112-100000%40peano.math.uni-bremen.de\">\r\n\
\   <META NAME=\"robots\" CONTENT=\"index,nofollow\">\r\n\
\   <META http-equiv=\"Content-Type\" content=\"text/html; charset=us-ascii\">\r\n\
\   <LINK REL=\"Previous\"  HREF=\"013910.html\">\r\n\
\   <LINK REL=\"Next\"  HREF=\"013901.html\">\r\n\
\ </HEAD>\r\n\
\ <BODY BGCOLOR=\"#ffffff\">\r\n\
\   <H1>[Haskell-cafe] multiline strings in haskell?</H1>\r\n\
\    <B>Sebastian Sylvan</B> \r\n\
\    <A HREF=\"mailto:haskell-cafe%40haskell.org?Subject=%5BHaskell-cafe%5D%20multiline%20strings%20in%20haskell%3F&In-Reply-To=Pine.LNX.4.44.0601121646300.23112-100000%40peano.math.uni-bremen.de\"\r\n\
\       TITLE=\"[Haskell-cafe] multiline strings in haskell?\">sebastian.sylvan at gmail.com\r\n\
\       </A><BR>\r\n\
\    <I>Thu Jan 12 11:04:43 EST 2006</I>\r\n\
\    <P><UL>\r\n\
\        <LI>Previous message: <A HREF=\"013910.html\">[Haskell-cafe] multiline strings in haskell?\r\n\
\</A></li>\r\n\
\        <LI>Next message: <A HREF=\"013901.html\">[Haskell-cafe] multiline strings in haskell?\r\n\
\</A></li>\r\n\
\         <LI> <B>Messages sorted by:</B> \r\n\
\              <a href=\"date.html#13911\">[ date ]</a>\r\n\
\              <a href=\"thread.html#13911\">[ thread ]</a>\r\n\
\              <a href=\"subject.html#13911\">[ subject ]</a>\r\n\
\              <a href=\"author.html#13911\">[ author ]</a>\r\n\
\         </LI>\r\n\
\       </UL>\r\n\
\    <HR>  \r\n\
\<!--beginarticle-->\r\n\
\<PRE>On 1/12/06, Henning Thielemann &lt;<A HREF=\"http://www.haskell.org/mailman/listinfo/haskell-cafe\">lemming at henning-thielemann.de</A>&gt; wrote:\r\n\
\&gt;<i>\r\n\
\</I>&gt;<i> On Thu, 12 Jan 2006, Jason Dagit wrote:\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> &gt; On Jan 12, 2006, at 1:34 AM, Henning Thielemann wrote:\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; &gt; On Wed, 11 Jan 2006, Michael Vanier wrote:\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt;&gt; Is there any support for multi-line string literals in Haskell?  I've\r\n\
\</I>&gt;<i> &gt; &gt;&gt; done a web search and come up empty.  I'm thinking of using\r\n\
\</I>&gt;<i> &gt; &gt;&gt; Haskell to\r\n\
\</I>&gt;<i> &gt; &gt;&gt; generate web pages and having multi-line strings would be very\r\n\
\</I>&gt;<i> &gt; &gt;&gt; useful.\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt; Do you mean\r\n\
\</I>&gt;<i> &gt; &gt;\r\n\
\</I>&gt;<i> &gt; &gt; unlines [&quot;first line&quot;, &quot;second line&quot;, &quot;third line&quot;]\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; The original poster probably meant something like this:\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; let foo = &quot;This is a\r\n\
\</I>&gt;<i> &gt; long string\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt;\r\n\
\</I>&gt;<i> &gt; Which does not end until the matching end quote.&quot;\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> I don't see the need for it, since\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> unlines [\r\n\
\</I>&gt;<i>   &quot;first line&quot;,\r\n\
\</I>&gt;<i>   &quot;second line&quot;,\r\n\
\</I>&gt;<i>   &quot;third line&quot;]\r\n\
\</I>&gt;<i>\r\n\
\</I>&gt;<i> works as well.\r\n\
\</I>&gt;<i>\r\n\
\</I>\r\n\
\Nevertheless Haskell supports multiline strings (although it seems\r\n\
\like a lot of people don't know about it). You escape it using  and\r\n\
\then another  where the string starts again.\r\n\
\\r\n\
\str = &quot;multi \r\n\
\        \\line&quot; \r\n\
\\r\n\
\Prelude&gt;str\r\n\
\&quot;multiline&quot;\r\n\
\\r\n\
\\r\n\
\/S\r\n\
\\r\n\
\--\r\n\
\Sebastian Sylvan\r\n\
\+46(0)736-818655\r\n\
\UIN: 44640862\r\n\
\</PRE>\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\\r\n\
\<!--endarticle-->\r\n\
\    <HR>\r\n\
\    <P><UL>\r\n\
\        <!--threads-->\r\n\

\<a href=\"http://www.haskell.org/mailman/listinfo/haskell-cafe\">More information about the Haskell-Cafe\r\n\
\mailing list</a><br>\r\n\
\</body></html>\r\n\
\\r\n"


shortResponse :: B.ByteString 
shortResponse = "<html><head>hello world!</head></html>\
\"