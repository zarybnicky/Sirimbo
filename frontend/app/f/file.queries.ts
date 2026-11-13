/** Types generated for queries found in "app/f/file.sql" */
import { PreparedQuery } from '@pgtyped/runtime';

export type NumberOrString = number | string;

/** 'FindDownloadFile' parameters type */
export interface IFindDownloadFileParams {
  id: NumberOrString;
}

/** 'FindDownloadFile' return type */
export interface IFindDownloadFileResult {
  byte_size: string | null;
  content_type: string | null;
  name: string;
  object_key: string;
}

/** 'FindDownloadFile' query type */
export interface IFindDownloadFileQuery {
  params: IFindDownloadFileParams;
  result: IFindDownloadFileResult;
}

const findDownloadFileIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":70,"b":73}]}],"statement":"select object_key, name, content_type, byte_size from file where id = :id!"};

/**
 * Query generated from SQL:
 * ```
 * select object_key, name, content_type, byte_size from file where id = :id!
 * ```
 */
export const findDownloadFile = new PreparedQuery<IFindDownloadFileParams,IFindDownloadFileResult>(findDownloadFileIR);


/** 'CreateUploadedFile' parameters type */
export interface ICreateUploadedFileParams {
  byteSize?: NumberOrString | null | void;
  contentType?: string | null | void;
  name: string;
  objectKey: string;
}

/** 'CreateUploadedFile' return type */
export interface ICreateUploadedFileResult {
  id: string;
  object_key: string;
  url: string;
}

/** 'CreateUploadedFile' query type */
export interface ICreateUploadedFileQuery {
  params: ICreateUploadedFileParams;
  result: ICreateUploadedFileResult;
}

const createUploadedFileIR: any = {"usedParamSet":{"objectKey":true,"name":true,"contentType":true,"byteSize":true},"params":[{"name":"objectKey","required":true,"transform":{"type":"scalar"},"locs":[{"a":69,"b":79}]},{"name":"name","required":true,"transform":{"type":"scalar"},"locs":[{"a":82,"b":87}]},{"name":"contentType","required":false,"transform":{"type":"scalar"},"locs":[{"a":90,"b":101}]},{"name":"byteSize","required":false,"transform":{"type":"scalar"},"locs":[{"a":104,"b":112}]}],"statement":"insert into file (object_key, name, content_type, byte_size)\nvalues (:objectKey!, :name!, :contentType, :byteSize)\nreturning id, object_key, url"};

/**
 * Query generated from SQL:
 * ```
 * insert into file (object_key, name, content_type, byte_size)
 * values (:objectKey!, :name!, :contentType, :byteSize)
 * returning id, object_key, url
 * ```
 */
export const createUploadedFile = new PreparedQuery<ICreateUploadedFileParams,ICreateUploadedFileResult>(createUploadedFileIR);


/** 'MarkFileUploaded' parameters type */
export interface IMarkFileUploadedParams {
  id: NumberOrString;
}

/** 'MarkFileUploaded' return type */
export type IMarkFileUploadedResult = void;

/** 'MarkFileUploaded' query type */
export interface IMarkFileUploadedQuery {
  params: IMarkFileUploadedParams;
  result: IMarkFileUploadedResult;
}

const markFileUploadedIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":47,"b":50}]}],"statement":"update file set uploaded_at = now() where id = :id!"};

/**
 * Query generated from SQL:
 * ```
 * update file set uploaded_at = now() where id = :id!
 * ```
 */
export const markFileUploaded = new PreparedQuery<IMarkFileUploadedParams,IMarkFileUploadedResult>(markFileUploadedIR);


/** 'DeleteUploadedFile' parameters type */
export interface IDeleteUploadedFileParams {
  id: NumberOrString;
}

/** 'DeleteUploadedFile' return type */
export type IDeleteUploadedFileResult = void;

/** 'DeleteUploadedFile' query type */
export interface IDeleteUploadedFileQuery {
  params: IDeleteUploadedFileParams;
  result: IDeleteUploadedFileResult;
}

const deleteUploadedFileIR: any = {"usedParamSet":{"id":true},"params":[{"name":"id","required":true,"transform":{"type":"scalar"},"locs":[{"a":28,"b":31}]}],"statement":"delete from file where id = :id!"};

/**
 * Query generated from SQL:
 * ```
 * delete from file where id = :id!
 * ```
 */
export const deleteUploadedFile = new PreparedQuery<IDeleteUploadedFileParams,IDeleteUploadedFileResult>(deleteUploadedFileIR);


