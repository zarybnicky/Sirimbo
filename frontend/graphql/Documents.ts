/* DO NOT EDIT! This file is auto-generated by graphql-code-generator - see `codegen.yml` */
/* eslint-disable */
import * as Types from './index';

import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type FileListQueryVariables = Types.Exact<{
  category?: Types.InputMaybe<Types.Scalars['Int']['input']>;
}>;


export type FileListQuery = { __typename?: 'Query', dokumentiesList: Array<{ __typename?: 'Dokumenty', id: string, dName: string, dFilename: string, dKategorie: number, dTimestamp: string | null, userByDKdo: { __typename?: 'User', id: string, uJmeno: string | null, uPrijmeni: string | null } | null }> | null };


export const FileListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"FileList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"category"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"dokumentiesList"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"dKategorie"},"value":{"kind":"Variable","name":{"kind":"Name","value":"category"}}}]}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"D_TIMESTAMP_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"dName"}},{"kind":"Field","name":{"kind":"Name","value":"dFilename"}},{"kind":"Field","name":{"kind":"Name","value":"dKategorie"}},{"kind":"Field","name":{"kind":"Name","value":"dTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"userByDKdo"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}}]}}]}}]} as unknown as DocumentNode<FileListQuery, FileListQueryVariables>;