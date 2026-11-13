/* @name FindDownloadFile */
select object_key, name, content_type, byte_size from file where id = :id!;

/* @name CreateUploadedFile */
insert into file (object_key, name, content_type, byte_size)
values (:objectKey!, :name!, :contentType, :byteSize)
returning id, object_key, url;

/* @name MarkFileUploaded */
update file set uploaded_at = now() where id = :id!;

/* @name DeleteUploadedFile */
delete from file where id = :id!;
