/* DO NOT EDIT! This file is auto-generated by graphql-code-generator - see `codegen.yml` */
/* eslint-disable */
import * as Types from './index';

import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type PaymentItemFragment = { __typename: 'PlatbyItem', piIdCategory: string, piIdRaw: string | null, piIdUser: string | null, piAmount: any, piPrefix: number, piDate: string, id: string, userByPiIdUser: { __typename?: 'User', uJmeno: string, uPrijmeni: string, id: string } | null, platbyCategoryByPiIdCategory: { __typename?: 'PlatbyCategory', pcName: string, id: string } | null };

export type PaymentItemListQueryVariables = Types.Exact<{
  first?: Types.InputMaybe<Types.Scalars['Int']['input']>;
  offset?: Types.InputMaybe<Types.Scalars['Int']['input']>;
}>;


export type PaymentItemListQuery = { __typename?: 'Query', platbyItems: { __typename?: 'PlatbyItemsConnection', totalCount: number, nodes: Array<{ __typename: 'PlatbyItem', piIdCategory: string, piIdRaw: string | null, piIdUser: string | null, piAmount: any, piPrefix: number, piDate: string, id: string, userByPiIdUser: { __typename?: 'User', uJmeno: string, uPrijmeni: string, id: string } | null, platbyCategoryByPiIdCategory: { __typename?: 'PlatbyCategory', pcName: string, id: string } | null }> } | null };

export type PaymentItemQueryVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type PaymentItemQuery = { __typename?: 'Query', platbyItem: { __typename: 'PlatbyItem', piIdCategory: string, piIdRaw: string | null, piIdUser: string | null, piAmount: any, piPrefix: number, piDate: string, id: string, userByPiIdUser: { __typename?: 'User', uJmeno: string, uPrijmeni: string, id: string } | null, platbyCategoryByPiIdCategory: { __typename?: 'PlatbyCategory', pcName: string, id: string } | null } | null };

export type PaymentCategoryFragment = { __typename: 'PlatbyCategory', pcName: string, pcSymbol: string, pcAmount: any, pcDateDue: string, pcValidFrom: string, pcValidTo: string, pcUsePrefix: boolean, pcUseBase: boolean, pcArchive: boolean, pcVisible: boolean, id: string };

export type PaymentCategoryListQueryVariables = Types.Exact<{
  first?: Types.InputMaybe<Types.Scalars['Int']['input']>;
  offset?: Types.InputMaybe<Types.Scalars['Int']['input']>;
}>;


export type PaymentCategoryListQuery = { __typename?: 'Query', platbyCategories: { __typename?: 'PlatbyCategoriesConnection', totalCount: number, nodes: Array<{ __typename: 'PlatbyCategory', pcName: string, pcSymbol: string, pcAmount: any, pcDateDue: string, pcValidFrom: string, pcValidTo: string, pcUsePrefix: boolean, pcUseBase: boolean, pcArchive: boolean, pcVisible: boolean, id: string }> } | null };

export type PaymentCategoryQueryVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type PaymentCategoryQuery = { __typename?: 'Query', platbyCategory: { __typename: 'PlatbyCategory', pcName: string, pcSymbol: string, pcAmount: any, pcDateDue: string, pcValidFrom: string, pcValidTo: string, pcUsePrefix: boolean, pcUseBase: boolean, pcArchive: boolean, pcVisible: boolean, id: string } | null };

export const PaymentItemFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentItem"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyItem"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"piId"}},{"kind":"Field","name":{"kind":"Name","value":"piIdCategory"}},{"kind":"Field","name":{"kind":"Name","value":"piIdRaw"}},{"kind":"Field","name":{"kind":"Name","value":"piIdUser"}},{"kind":"Field","name":{"kind":"Name","value":"piAmount"}},{"kind":"Field","name":{"kind":"Name","value":"piPrefix"}},{"kind":"Field","name":{"kind":"Name","value":"piDate"}},{"kind":"Field","name":{"kind":"Name","value":"userByPiIdUser"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"platbyCategoryByPiIdCategory"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}}]}}]}}]} as unknown as DocumentNode<PaymentItemFragment, unknown>;
export const PaymentCategoryFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentCategory"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyCategory"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}},{"kind":"Field","name":{"kind":"Name","value":"pcSymbol"}},{"kind":"Field","name":{"kind":"Name","value":"pcAmount"}},{"kind":"Field","name":{"kind":"Name","value":"pcDateDue"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidFrom"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidTo"}},{"kind":"Field","name":{"kind":"Name","value":"pcUsePrefix"}},{"kind":"Field","name":{"kind":"Name","value":"pcUseBase"}},{"kind":"Field","name":{"kind":"Name","value":"pcArchive"}},{"kind":"Field","name":{"kind":"Name","value":"pcVisible"}}]}}]} as unknown as DocumentNode<PaymentCategoryFragment, unknown>;
export const PaymentItemListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"PaymentItemList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"first"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"platbyItems"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"first"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"PaymentItem"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentItem"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyItem"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"piId"}},{"kind":"Field","name":{"kind":"Name","value":"piIdCategory"}},{"kind":"Field","name":{"kind":"Name","value":"piIdRaw"}},{"kind":"Field","name":{"kind":"Name","value":"piIdUser"}},{"kind":"Field","name":{"kind":"Name","value":"piAmount"}},{"kind":"Field","name":{"kind":"Name","value":"piPrefix"}},{"kind":"Field","name":{"kind":"Name","value":"piDate"}},{"kind":"Field","name":{"kind":"Name","value":"userByPiIdUser"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"platbyCategoryByPiIdCategory"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}}]}}]}}]} as unknown as DocumentNode<PaymentItemListQuery, PaymentItemListQueryVariables>;
export const PaymentItemDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"PaymentItem"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"platbyItem"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"piId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"PaymentItem"}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentItem"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyItem"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"piId"}},{"kind":"Field","name":{"kind":"Name","value":"piIdCategory"}},{"kind":"Field","name":{"kind":"Name","value":"piIdRaw"}},{"kind":"Field","name":{"kind":"Name","value":"piIdUser"}},{"kind":"Field","name":{"kind":"Name","value":"piAmount"}},{"kind":"Field","name":{"kind":"Name","value":"piPrefix"}},{"kind":"Field","name":{"kind":"Name","value":"piDate"}},{"kind":"Field","name":{"kind":"Name","value":"userByPiIdUser"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"platbyCategoryByPiIdCategory"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}}]}}]}}]} as unknown as DocumentNode<PaymentItemQuery, PaymentItemQueryVariables>;
export const PaymentCategoryListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"PaymentCategoryList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"first"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"platbyCategories"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"first"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"PaymentCategory"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentCategory"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyCategory"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}},{"kind":"Field","name":{"kind":"Name","value":"pcSymbol"}},{"kind":"Field","name":{"kind":"Name","value":"pcAmount"}},{"kind":"Field","name":{"kind":"Name","value":"pcDateDue"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidFrom"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidTo"}},{"kind":"Field","name":{"kind":"Name","value":"pcUsePrefix"}},{"kind":"Field","name":{"kind":"Name","value":"pcUseBase"}},{"kind":"Field","name":{"kind":"Name","value":"pcArchive"}},{"kind":"Field","name":{"kind":"Name","value":"pcVisible"}}]}}]} as unknown as DocumentNode<PaymentCategoryListQuery, PaymentCategoryListQueryVariables>;
export const PaymentCategoryDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"PaymentCategory"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"platbyCategory"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"pcId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"PaymentCategory"}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"PaymentCategory"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"PlatbyCategory"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"pcId"}},{"kind":"Field","name":{"kind":"Name","value":"pcName"}},{"kind":"Field","name":{"kind":"Name","value":"pcSymbol"}},{"kind":"Field","name":{"kind":"Name","value":"pcAmount"}},{"kind":"Field","name":{"kind":"Name","value":"pcDateDue"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidFrom"}},{"kind":"Field","name":{"kind":"Name","value":"pcValidTo"}},{"kind":"Field","name":{"kind":"Name","value":"pcUsePrefix"}},{"kind":"Field","name":{"kind":"Name","value":"pcUseBase"}},{"kind":"Field","name":{"kind":"Name","value":"pcArchive"}},{"kind":"Field","name":{"kind":"Name","value":"pcVisible"}}]}}]} as unknown as DocumentNode<PaymentCategoryQuery, PaymentCategoryQueryVariables>;