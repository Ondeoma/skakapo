-- 添付は画像に限らず、どの種類のファイルでもよくする（Claude に渡せない種類は、名前と種類だけを伝える）
alter table attachment drop constraint attachment_content_type_check;
