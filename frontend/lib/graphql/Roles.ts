/* DO NOT EDIT! This file is auto-generated by graphql-code-generator - see `codegen.yml` */
/* eslint-disable */
import * as Types from './index';

import { useQuery, useMutation, UseQueryOptions, UseMutationOptions } from '@tanstack/react-query';
import { fetcher } from 'lib/query';
export type RoleFragment = { __typename: 'Permission', peAkce: number, peAnkety: number, peAktuality: number, peDescription: string, peDokumenty: number, peGalerie: number, peId: string, peKonzole: number, peInzerce: number, peNabidka: number, peMain: number, peName: string, peNastenka: number, peNovinky: number, pePary: number, pePermissions: number, pePlatby: number, peRozpis: number, peSkupiny: number, peUsers: number };

export type RoleListQueryVariables = Types.Exact<{ [key: string]: never; }>;


export type RoleListQuery = { __typename?: 'Query', permissions: { __typename?: 'PermissionsConnection', totalCount: number, nodes: Array<{ __typename: 'Permission', peAkce: number, peAnkety: number, peAktuality: number, peDescription: string, peDokumenty: number, peGalerie: number, peId: string, peKonzole: number, peInzerce: number, peNabidka: number, peMain: number, peName: string, peNastenka: number, peNovinky: number, pePary: number, pePermissions: number, pePlatby: number, peRozpis: number, peSkupiny: number, peUsers: number }> } | null };

export type RoleQueryVariables = Types.Exact<{
  id: Types.Scalars['BigInt'];
}>;


export type RoleQuery = { __typename?: 'Query', permission: { __typename: 'Permission', peAkce: number, peAnkety: number, peAktuality: number, peDescription: string, peDokumenty: number, peGalerie: number, peId: string, peKonzole: number, peInzerce: number, peNabidka: number, peMain: number, peName: string, peNastenka: number, peNovinky: number, pePary: number, pePermissions: number, pePlatby: number, peRozpis: number, peSkupiny: number, peUsers: number } | null };

export type CreateRoleMutationVariables = Types.Exact<{
  input: Types.PermissionInput;
}>;


export type CreateRoleMutation = { __typename?: 'Mutation', createPermission: { __typename?: 'CreatePermissionPayload', permission: { __typename?: 'Permission', peId: string } | null } | null };

export type UpdateRoleMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt'];
  patch: Types.PermissionPatch;
}>;


export type UpdateRoleMutation = { __typename?: 'Mutation', updatePermission: { __typename: 'UpdatePermissionPayload' } | null };

export type DeleteRoleMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt'];
}>;


export type DeleteRoleMutation = { __typename?: 'Mutation', deletePermission: { __typename: 'DeletePermissionPayload' } | null };

export const RoleFragmentDoc = `
    fragment Role on Permission {
  __typename
  peAkce
  peAnkety
  peAktuality
  peDescription
  peDokumenty
  peGalerie
  peId
  peKonzole
  peInzerce
  peNabidka
  peMain
  peName
  peNastenka
  peNovinky
  pePary
  pePermissions
  pePlatby
  peRozpis
  peSkupiny
  peUsers
}
    `;
export const RoleListDocument = `
    query RoleList {
  permissions {
    totalCount
    nodes {
      ...Role
    }
  }
}
    ${RoleFragmentDoc}`;
export const useRoleListQuery = <
      TData = RoleListQuery,
      TError = unknown
    >(
      variables?: RoleListQueryVariables,
      options?: UseQueryOptions<RoleListQuery, TError, TData>
    ) =>
    useQuery<RoleListQuery, TError, TData>(
      variables === undefined ? ['RoleList'] : ['RoleList', variables],
      fetcher<RoleListQuery, RoleListQueryVariables>(RoleListDocument, variables),
      options
    );

useRoleListQuery.getKey = (variables?: RoleListQueryVariables) => variables === undefined ? ['RoleList'] : ['RoleList', variables];
;

useRoleListQuery.fetcher = (variables?: RoleListQueryVariables, options?: RequestInit['headers']) => fetcher<RoleListQuery, RoleListQueryVariables>(RoleListDocument, variables, options);
export const RoleDocument = `
    query Role($id: BigInt!) {
  permission(peId: $id) {
    ...Role
  }
}
    ${RoleFragmentDoc}`;
export const useRoleQuery = <
      TData = RoleQuery,
      TError = unknown
    >(
      variables: RoleQueryVariables,
      options?: UseQueryOptions<RoleQuery, TError, TData>
    ) =>
    useQuery<RoleQuery, TError, TData>(
      ['Role', variables],
      fetcher<RoleQuery, RoleQueryVariables>(RoleDocument, variables),
      options
    );

useRoleQuery.getKey = (variables: RoleQueryVariables) => ['Role', variables];
;

useRoleQuery.fetcher = (variables: RoleQueryVariables, options?: RequestInit['headers']) => fetcher<RoleQuery, RoleQueryVariables>(RoleDocument, variables, options);
export const CreateRoleDocument = `
    mutation CreateRole($input: PermissionInput!) {
  createPermission(input: {permission: $input}) {
    permission {
      peId
    }
  }
}
    `;
export const useCreateRoleMutation = <
      TError = unknown,
      TContext = unknown
    >(options?: UseMutationOptions<CreateRoleMutation, TError, CreateRoleMutationVariables, TContext>) =>
    useMutation<CreateRoleMutation, TError, CreateRoleMutationVariables, TContext>(
      ['CreateRole'],
      (variables?: CreateRoleMutationVariables) => fetcher<CreateRoleMutation, CreateRoleMutationVariables>(CreateRoleDocument, variables)(),
      options
    );
useCreateRoleMutation.getKey = () => ['CreateRole'];

useCreateRoleMutation.fetcher = (variables: CreateRoleMutationVariables, options?: RequestInit['headers']) => fetcher<CreateRoleMutation, CreateRoleMutationVariables>(CreateRoleDocument, variables, options);
export const UpdateRoleDocument = `
    mutation UpdateRole($id: BigInt!, $patch: PermissionPatch!) {
  updatePermission(input: {peId: $id, patch: $patch}) {
    __typename
  }
}
    `;
export const useUpdateRoleMutation = <
      TError = unknown,
      TContext = unknown
    >(options?: UseMutationOptions<UpdateRoleMutation, TError, UpdateRoleMutationVariables, TContext>) =>
    useMutation<UpdateRoleMutation, TError, UpdateRoleMutationVariables, TContext>(
      ['UpdateRole'],
      (variables?: UpdateRoleMutationVariables) => fetcher<UpdateRoleMutation, UpdateRoleMutationVariables>(UpdateRoleDocument, variables)(),
      options
    );
useUpdateRoleMutation.getKey = () => ['UpdateRole'];

useUpdateRoleMutation.fetcher = (variables: UpdateRoleMutationVariables, options?: RequestInit['headers']) => fetcher<UpdateRoleMutation, UpdateRoleMutationVariables>(UpdateRoleDocument, variables, options);
export const DeleteRoleDocument = `
    mutation DeleteRole($id: BigInt!) {
  deletePermission(input: {peId: $id}) {
    __typename
  }
}
    `;
export const useDeleteRoleMutation = <
      TError = unknown,
      TContext = unknown
    >(options?: UseMutationOptions<DeleteRoleMutation, TError, DeleteRoleMutationVariables, TContext>) =>
    useMutation<DeleteRoleMutation, TError, DeleteRoleMutationVariables, TContext>(
      ['DeleteRole'],
      (variables?: DeleteRoleMutationVariables) => fetcher<DeleteRoleMutation, DeleteRoleMutationVariables>(DeleteRoleDocument, variables)(),
      options
    );
useDeleteRoleMutation.getKey = () => ['DeleteRole'];

useDeleteRoleMutation.fetcher = (variables: DeleteRoleMutationVariables, options?: RequestInit['headers']) => fetcher<DeleteRoleMutation, DeleteRoleMutationVariables>(DeleteRoleDocument, variables, options);