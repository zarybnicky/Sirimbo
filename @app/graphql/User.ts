/* DO NOT EDIT! This file is auto-generated by graphql-code-generator - see `codegen.yml` */
/* eslint-disable */
import * as Types from './index';

import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type UserPublicFragment = { __typename?: 'User', uJmeno: string, uPrijmeni: string, uRodneCislo: string | null, uTelefon: string, uEmail: string, id: string };

export type UserQueryVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type UserQuery = { __typename?: 'Query', user: { __typename?: 'User', uLogin: string, uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uConfirmed: boolean, uTeacher: boolean, uDancer: boolean, uSystem: boolean, uLock: boolean, uBan: boolean, uGroup: string, uSkupina: string, uTimestamp: string, uStreet: string, uRodneCislo: string | null, uPoznamky: string, uPostalCode: string, uPohlavi: string, uOrientationNumber: string, uNationality: string, uNarozeni: string, uMemberUntil: string | null, uMemberSince: string | null, uGdprSignedAt: string | null, uDistrict: string, uCreatedAt: string, uConscriptionNumber: string, uCity: string, id: string } | null };

export type UserListQueryVariables = Types.Exact<{
  confirmed?: Types.InputMaybe<Types.Scalars['Boolean']['input']>;
  system?: Types.InputMaybe<Types.Scalars['Boolean']['input']>;
  ban?: Types.InputMaybe<Types.Scalars['Boolean']['input']>;
  first?: Types.InputMaybe<Types.Scalars['Int']['input']>;
  offset?: Types.InputMaybe<Types.Scalars['Int']['input']>;
}>;


export type UserListQuery = { __typename?: 'Query', users: { __typename?: 'UsersConnection', totalCount: number, nodes: Array<{ __typename?: 'User', uLogin: string, uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uConfirmed: boolean, uTeacher: boolean, uDancer: boolean, uSystem: boolean, uLock: boolean, uBan: boolean, uGroup: string, uSkupina: string, uTimestamp: string, uStreet: string, uRodneCislo: string | null, uPoznamky: string, uPostalCode: string, uPohlavi: string, uOrientationNumber: string, uNationality: string, uNarozeni: string, uMemberUntil: string | null, uMemberSince: string | null, uGdprSignedAt: string | null, uDistrict: string, uCreatedAt: string, uConscriptionNumber: string, uCity: string, id: string }> } | null };

export type MsmtExportQueryVariables = Types.Exact<{ [key: string]: never; }>;


export type MsmtExportQuery = { __typename?: 'Query', users: { __typename?: 'UsersConnection', nodes: Array<{ __typename?: 'User', uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uRodneCislo: string | null, uNationality: string, uNarozeni: string, uCity: string, uDistrict: string, uConscriptionNumber: string, uOrientationNumber: string, uPostalCode: string, uStreet: string, uGroup: string, hasValidPayment: boolean | null, dateOfOldestPayment: string | null, dateOfNewestPayment: string | null, id: string, skupinyByUSkupina: { __typename?: 'Skupiny', sId: string, sName: string, sColorRgb: string, sDescription: string, sVisible: boolean } | null }> } | null };

export type MemberListQueryVariables = Types.Exact<{
  cohortId?: Types.InputMaybe<Types.Scalars['BigInt']['input']>;
}>;


export type MemberListQuery = { __typename?: 'Query', users: { __typename?: 'UsersConnection', totalCount: number, nodes: Array<{ __typename?: 'User', uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uRodneCislo: string | null, hasValidPayment: boolean | null, id: string, skupinyByUSkupina: { __typename?: 'Skupiny', sId: string, sName: string, sColorRgb: string, sDescription: string, sVisible: boolean } | null }> } | null };

export type TrainerListQueryVariables = Types.Exact<{ [key: string]: never; }>;


export type TrainerListQuery = { __typename?: 'Query', trainers: { __typename?: 'UsersConnection', totalCount: number, nodes: Array<{ __typename?: 'User', uLogin: string, uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uConfirmed: boolean, uTeacher: boolean, uDancer: boolean, uSystem: boolean, uLock: boolean, uBan: boolean, uGroup: string, uSkupina: string, uTimestamp: string, uStreet: string, uRodneCislo: string | null, uPoznamky: string, uPostalCode: string, uPohlavi: string, uOrientationNumber: string, uNationality: string, uNarozeni: string, uMemberUntil: string | null, uMemberSince: string | null, uGdprSignedAt: string | null, uDistrict: string, uCreatedAt: string, uConscriptionNumber: string, uCity: string, id: string }> } | null };

export type ConfirmUserMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
  role: Types.Scalars['BigInt']['input'];
  cohort: Types.Scalars['BigInt']['input'];
}>;


export type ConfirmUserMutation = { __typename?: 'Mutation', confirmUser: { __typename: 'ConfirmUserPayload' } | null };

export type CreateUserMutationVariables = Types.Exact<{
  input: Types.UserInput;
}>;


export type CreateUserMutation = { __typename?: 'Mutation', createUser: { __typename?: 'CreateUserPayload', user: { __typename?: 'User', uLogin: string, uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uConfirmed: boolean, uTeacher: boolean, uDancer: boolean, uSystem: boolean, uLock: boolean, uBan: boolean, uGroup: string, uSkupina: string, uTimestamp: string, uStreet: string, uRodneCislo: string | null, uPoznamky: string, uPostalCode: string, uPohlavi: string, uOrientationNumber: string, uNationality: string, uNarozeni: string, uMemberUntil: string | null, uMemberSince: string | null, uGdprSignedAt: string | null, uDistrict: string, uCreatedAt: string, uConscriptionNumber: string, uCity: string, id: string } | null } | null };

export type UpdateUserMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
  patch: Types.UserPatch;
}>;


export type UpdateUserMutation = { __typename?: 'Mutation', updateUser: { __typename?: 'UpdateUserPayload', user: { __typename?: 'User', uLogin: string, uJmeno: string, uPrijmeni: string, uEmail: string, uTelefon: string, uConfirmed: boolean, uTeacher: boolean, uDancer: boolean, uSystem: boolean, uLock: boolean, uBan: boolean, uGroup: string, uSkupina: string, uTimestamp: string, uStreet: string, uRodneCislo: string | null, uPoznamky: string, uPostalCode: string, uPohlavi: string, uOrientationNumber: string, uNationality: string, uNarozeni: string, uMemberUntil: string | null, uMemberSince: string | null, uGdprSignedAt: string | null, uDistrict: string, uCreatedAt: string, uConscriptionNumber: string, uCity: string, id: string } | null } | null };

export type DeleteUserMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type DeleteUserMutation = { __typename?: 'Mutation', deleteUser: { __typename: 'DeleteUserPayload' } | null };

export const UserPublicFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"UserPublic"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}}]}}]} as unknown as DocumentNode<UserPublicFragment, unknown>;
export const UserDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"User"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"user"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"uId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"User"}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"User"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uLogin"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uConfirmed"}},{"kind":"Field","name":{"kind":"Name","value":"uTeacher"}},{"kind":"Field","name":{"kind":"Name","value":"uDancer"}},{"kind":"Field","name":{"kind":"Name","value":"uSystem"}},{"kind":"Field","name":{"kind":"Name","value":"uLock"}},{"kind":"Field","name":{"kind":"Name","value":"uBan"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"uSkupina"}},{"kind":"Field","name":{"kind":"Name","value":"uTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uPoznamky"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uPohlavi"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberUntil"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberSince"}},{"kind":"Field","name":{"kind":"Name","value":"uGdprSignedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uCreatedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}}]}}]} as unknown as DocumentNode<UserQuery, UserQueryVariables>;
export const UserListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"UserList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"confirmed"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"system"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}},"defaultValue":{"kind":"BooleanValue","value":false}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"ban"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}},"defaultValue":{"kind":"BooleanValue","value":false}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"first"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"users"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"uBan"},"value":{"kind":"Variable","name":{"kind":"Name","value":"ban"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"uSystem"},"value":{"kind":"Variable","name":{"kind":"Name","value":"system"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"uConfirmed"},"value":{"kind":"Variable","name":{"kind":"Name","value":"confirmed"}}}]}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"first"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"User"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"User"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uLogin"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uConfirmed"}},{"kind":"Field","name":{"kind":"Name","value":"uTeacher"}},{"kind":"Field","name":{"kind":"Name","value":"uDancer"}},{"kind":"Field","name":{"kind":"Name","value":"uSystem"}},{"kind":"Field","name":{"kind":"Name","value":"uLock"}},{"kind":"Field","name":{"kind":"Name","value":"uBan"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"uSkupina"}},{"kind":"Field","name":{"kind":"Name","value":"uTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uPoznamky"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uPohlavi"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberUntil"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberSince"}},{"kind":"Field","name":{"kind":"Name","value":"uGdprSignedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uCreatedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}}]}}]} as unknown as DocumentNode<UserListQuery, UserListQueryVariables>;
export const MsmtExportDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"MsmtExport"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"users"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"uConfirmed"},"value":{"kind":"BooleanValue","value":true}},{"kind":"ObjectField","name":{"kind":"Name","value":"uSystem"},"value":{"kind":"BooleanValue","value":false}},{"kind":"ObjectField","name":{"kind":"Name","value":"uBan"},"value":{"kind":"BooleanValue","value":false}},{"kind":"ObjectField","name":{"kind":"Name","value":"inPublicCohort"},"value":{"kind":"BooleanValue","value":true}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"skupinyByUSkupina"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"sId"}},{"kind":"Field","name":{"kind":"Name","value":"sName"}},{"kind":"Field","name":{"kind":"Name","value":"sColorRgb"}},{"kind":"Field","name":{"kind":"Name","value":"sDescription"}},{"kind":"Field","name":{"kind":"Name","value":"sVisible"}}]}},{"kind":"Field","name":{"kind":"Name","value":"hasValidPayment"}},{"kind":"Field","name":{"kind":"Name","value":"dateOfOldestPayment"}},{"kind":"Field","name":{"kind":"Name","value":"dateOfNewestPayment"}}]}}]}}]}}]} as unknown as DocumentNode<MsmtExportQuery, MsmtExportQueryVariables>;
export const MemberListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"MemberList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"cohortId"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"users"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"uConfirmed"},"value":{"kind":"BooleanValue","value":true}},{"kind":"ObjectField","name":{"kind":"Name","value":"uSystem"},"value":{"kind":"BooleanValue","value":false}},{"kind":"ObjectField","name":{"kind":"Name","value":"uBan"},"value":{"kind":"BooleanValue","value":false}},{"kind":"ObjectField","name":{"kind":"Name","value":"uSkupina"},"value":{"kind":"Variable","name":{"kind":"Name","value":"cohortId"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"skupinyByUSkupina"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"sId"}},{"kind":"Field","name":{"kind":"Name","value":"sName"}},{"kind":"Field","name":{"kind":"Name","value":"sColorRgb"}},{"kind":"Field","name":{"kind":"Name","value":"sDescription"}},{"kind":"Field","name":{"kind":"Name","value":"sVisible"}}]}},{"kind":"Field","name":{"kind":"Name","value":"hasValidPayment"}}]}}]}}]}}]} as unknown as DocumentNode<MemberListQuery, MemberListQueryVariables>;
export const TrainerListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"TrainerList"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"trainers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"User"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"User"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uLogin"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uConfirmed"}},{"kind":"Field","name":{"kind":"Name","value":"uTeacher"}},{"kind":"Field","name":{"kind":"Name","value":"uDancer"}},{"kind":"Field","name":{"kind":"Name","value":"uSystem"}},{"kind":"Field","name":{"kind":"Name","value":"uLock"}},{"kind":"Field","name":{"kind":"Name","value":"uBan"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"uSkupina"}},{"kind":"Field","name":{"kind":"Name","value":"uTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uPoznamky"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uPohlavi"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberUntil"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberSince"}},{"kind":"Field","name":{"kind":"Name","value":"uGdprSignedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uCreatedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}}]}}]} as unknown as DocumentNode<TrainerListQuery, TrainerListQueryVariables>;
export const ConfirmUserDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"ConfirmUser"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"role"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"cohort"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"confirmUser"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"grp"},"value":{"kind":"Variable","name":{"kind":"Name","value":"role"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"cohort"},"value":{"kind":"Variable","name":{"kind":"Name","value":"cohort"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<ConfirmUserMutation, ConfirmUserMutationVariables>;
export const CreateUserDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"CreateUser"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"input"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"UserInput"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"createUser"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"user"},"value":{"kind":"Variable","name":{"kind":"Name","value":"input"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"User"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"User"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uLogin"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uConfirmed"}},{"kind":"Field","name":{"kind":"Name","value":"uTeacher"}},{"kind":"Field","name":{"kind":"Name","value":"uDancer"}},{"kind":"Field","name":{"kind":"Name","value":"uSystem"}},{"kind":"Field","name":{"kind":"Name","value":"uLock"}},{"kind":"Field","name":{"kind":"Name","value":"uBan"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"uSkupina"}},{"kind":"Field","name":{"kind":"Name","value":"uTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uPoznamky"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uPohlavi"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberUntil"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberSince"}},{"kind":"Field","name":{"kind":"Name","value":"uGdprSignedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uCreatedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}}]}}]} as unknown as DocumentNode<CreateUserMutation, CreateUserMutationVariables>;
export const UpdateUserDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"UpdateUser"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"patch"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"UserPatch"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateUser"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"uId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"patch"},"value":{"kind":"Variable","name":{"kind":"Name","value":"patch"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"User"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"User"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"User"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"id"},"name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uLogin"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uConfirmed"}},{"kind":"Field","name":{"kind":"Name","value":"uTeacher"}},{"kind":"Field","name":{"kind":"Name","value":"uDancer"}},{"kind":"Field","name":{"kind":"Name","value":"uSystem"}},{"kind":"Field","name":{"kind":"Name","value":"uLock"}},{"kind":"Field","name":{"kind":"Name","value":"uBan"}},{"kind":"Field","name":{"kind":"Name","value":"uGroup"}},{"kind":"Field","name":{"kind":"Name","value":"uSkupina"}},{"kind":"Field","name":{"kind":"Name","value":"uTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"uStreet"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uPoznamky"}},{"kind":"Field","name":{"kind":"Name","value":"uPostalCode"}},{"kind":"Field","name":{"kind":"Name","value":"uPohlavi"}},{"kind":"Field","name":{"kind":"Name","value":"uOrientationNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uNationality"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberUntil"}},{"kind":"Field","name":{"kind":"Name","value":"uMemberSince"}},{"kind":"Field","name":{"kind":"Name","value":"uGdprSignedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uDistrict"}},{"kind":"Field","name":{"kind":"Name","value":"uCreatedAt"}},{"kind":"Field","name":{"kind":"Name","value":"uConscriptionNumber"}},{"kind":"Field","name":{"kind":"Name","value":"uCity"}}]}}]} as unknown as DocumentNode<UpdateUserMutation, UpdateUserMutationVariables>;
export const DeleteUserDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"DeleteUser"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"deleteUser"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"uId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<DeleteUserMutation, DeleteUserMutationVariables>;