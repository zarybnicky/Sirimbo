/* DO NOT EDIT! This file is auto-generated by graphql-code-generator - see `codegen.yml` */
/* eslint-disable */
import * as Types from './index';

import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type EventFragment = { __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean };

export type EventItemFragment = { __typename?: 'AttendeeUser', id: string, notes: string, user: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uRodneCislo: string | null, uTelefon: string, uEmail: string } | null };

export type EventWithItemsFragment = { __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean, attendeeExternals: { __typename?: 'AttendeeExternalsConnection', nodes: Array<{ __typename?: 'AttendeeExternal', firstName: string, lastName: string }> }, attendeeUsers: { __typename?: 'AttendeeUsersConnection', nodes: Array<{ __typename?: 'AttendeeUser', notes: string, user: { __typename?: 'User', uId: string, uJmeno: string, uPrijmeni: string, uNarozeni: string, uTelefon: string, uEmail: string, uRodneCislo: string | null } | null }> } };

export type EventQueryVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type EventQuery = { __typename?: 'Query', event: { __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean, attendeeExternals: { __typename?: 'AttendeeExternalsConnection', nodes: Array<{ __typename?: 'AttendeeExternal', firstName: string, lastName: string }> }, attendeeUsers: { __typename?: 'AttendeeUsersConnection', nodes: Array<{ __typename?: 'AttendeeUser', notes: string, user: { __typename?: 'User', uId: string, uJmeno: string, uPrijmeni: string, uNarozeni: string, uTelefon: string, uEmail: string, uRodneCislo: string | null } | null }> } } | null };

export type EventListQueryVariables = Types.Exact<{
  first?: Types.InputMaybe<Types.Scalars['Int']['input']>;
  offset?: Types.InputMaybe<Types.Scalars['Int']['input']>;
  visible?: Types.InputMaybe<Types.Scalars['Boolean']['input']>;
}>;


export type EventListQuery = { __typename?: 'Query', events: { __typename?: 'EventsConnection', totalCount: number, nodes: Array<{ __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean, attendeeExternals: { __typename?: 'AttendeeExternalsConnection', nodes: Array<{ __typename?: 'AttendeeExternal', firstName: string, lastName: string }> }, attendeeUsers: { __typename?: 'AttendeeUsersConnection', nodes: Array<{ __typename?: 'AttendeeUser', notes: string, user: { __typename?: 'User', uId: string, uJmeno: string, uPrijmeni: string, uNarozeni: string, uTelefon: string, uEmail: string, uRodneCislo: string | null } | null }> } }> } | null };

export type ToggleEventVisibleMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
  visible: Types.Scalars['Boolean']['input'];
}>;


export type ToggleEventVisibleMutation = { __typename?: 'Mutation', updateEvent: { __typename?: 'UpdateEventPayload', event: { __typename?: 'Event', id: string } | null } | null };

export type CreateEventMutationVariables = Types.Exact<{
  input: Types.EventInput;
}>;


export type CreateEventMutation = { __typename?: 'Mutation', createEvent: { __typename?: 'CreateEventPayload', event: { __typename?: 'Event', id: string } | null } | null };

export type UpdateEventMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
  patch: Types.EventPatch;
}>;


export type UpdateEventMutation = { __typename?: 'Mutation', updateEvent: { __typename: 'UpdateEventPayload' } | null };

export type DeleteEventMutationVariables = Types.Exact<{
  id: Types.Scalars['BigInt']['input'];
}>;


export type DeleteEventMutation = { __typename?: 'Mutation', deleteEvent: { __typename: 'DeleteEventPayload' } | null };

export type MyEventFragment = { __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean, attendeeExternals: { __typename?: 'AttendeeExternalsConnection', nodes: Array<{ __typename?: 'AttendeeExternal', firstName: string, lastName: string }> }, attendeeUsers: { __typename?: 'AttendeeUsersConnection', nodes: Array<{ __typename?: 'AttendeeUser', notes: string, user: { __typename?: 'User', uId: string, uJmeno: string, uPrijmeni: string, uNarozeni: string, uTelefon: string, uEmail: string, uRodneCislo: string | null } | null }> } };

export type MyEventsQueryVariables = Types.Exact<{ [key: string]: never; }>;


export type MyEventsQuery = { __typename?: 'Query', events: { __typename?: 'EventsConnection', nodes: Array<{ __typename: 'Event', id: string, since: string, until: string, summary: string, description: string, filesLegacy: string, name: string, capacity: string, remainingSpots: number | null, locationText: string, isLocked: boolean, isVisible: boolean, isPublic: boolean, enableNotes: boolean, attendeeExternals: { __typename?: 'AttendeeExternalsConnection', nodes: Array<{ __typename?: 'AttendeeExternal', firstName: string, lastName: string }> }, attendeeUsers: { __typename?: 'AttendeeUsersConnection', nodes: Array<{ __typename?: 'AttendeeUser', notes: string, user: { __typename?: 'User', uId: string, uJmeno: string, uPrijmeni: string, uNarozeni: string, uTelefon: string, uEmail: string, uRodneCislo: string | null } | null }> } }> } | null };

export type CreateAttendeeExternalMutationVariables = Types.Exact<{
  input: Types.CreateParticipationExternalInput;
}>;


export type CreateAttendeeExternalMutation = { __typename?: 'Mutation', createParticipationExternal: { __typename: 'CreateParticipationExternalPayload' } | null };

export type CreateParticipationMutationVariables = Types.Exact<{
  input: Types.CreateParticipationInput;
}>;


export type CreateParticipationMutation = { __typename?: 'Mutation', createParticipation: { __typename: 'CreateParticipationPayload' } | null };

export type CancelParticipationMutationVariables = Types.Exact<{
  input: Types.CancelParticipationInput;
}>;


export type CancelParticipationMutation = { __typename?: 'Mutation', cancelParticipation: { __typename: 'CancelParticipationPayload' } | null };

export const EventItemFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventItem"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"AttendeeUser"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}}]}}]}}]} as unknown as DocumentNode<EventItemFragment, unknown>;
export const EventFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}}]} as unknown as DocumentNode<EventFragment, unknown>;
export const EventWithItemsFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventWithItems"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"Event"}},{"kind":"Field","name":{"kind":"Name","value":"attendeeExternals"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"firstName"}},{"kind":"Field","name":{"kind":"Name","value":"lastName"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"attendeeUsers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}}]}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}}]} as unknown as DocumentNode<EventWithItemsFragment, unknown>;
export const MyEventFragmentDoc = {"kind":"Document","definitions":[{"kind":"FragmentDefinition","name":{"kind":"Name","value":"MyEvent"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"EventWithItems"}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventWithItems"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"Event"}},{"kind":"Field","name":{"kind":"Name","value":"attendeeExternals"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"firstName"}},{"kind":"Field","name":{"kind":"Name","value":"lastName"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"attendeeUsers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}}]}}]}}]}}]}}]} as unknown as DocumentNode<MyEventFragment, unknown>;
export const EventDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"Event"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"event"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"EventWithItems"}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventWithItems"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"Event"}},{"kind":"Field","name":{"kind":"Name","value":"attendeeExternals"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"firstName"}},{"kind":"Field","name":{"kind":"Name","value":"lastName"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"attendeeUsers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}}]}}]}}]}}]}}]} as unknown as DocumentNode<EventQuery, EventQueryVariables>;
export const EventListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"EventList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"first"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"events"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"first"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"ListValue","values":[{"kind":"EnumValue","value":"SINCE_DESC"}]}},{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"isVisible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"totalCount"}},{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"EventWithItems"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventWithItems"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"Event"}},{"kind":"Field","name":{"kind":"Name","value":"attendeeExternals"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"firstName"}},{"kind":"Field","name":{"kind":"Name","value":"lastName"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"attendeeUsers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}}]}}]}}]}}]}}]} as unknown as DocumentNode<EventListQuery, EventListQueryVariables>;
export const ToggleEventVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"ToggleEventVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateEvent"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"patch"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"isVisible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"event"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}}]}}]}}]}}]} as unknown as DocumentNode<ToggleEventVisibleMutation, ToggleEventVisibleMutationVariables>;
export const CreateEventDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"CreateEvent"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"input"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"EventInput"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"createEvent"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"event"},"value":{"kind":"Variable","name":{"kind":"Name","value":"input"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"event"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"id"}}]}}]}}]}}]} as unknown as DocumentNode<CreateEventMutation, CreateEventMutationVariables>;
export const UpdateEventDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"UpdateEvent"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"patch"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"EventPatch"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateEvent"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"patch"},"value":{"kind":"Variable","name":{"kind":"Name","value":"patch"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<UpdateEventMutation, UpdateEventMutationVariables>;
export const DeleteEventDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"DeleteEvent"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"deleteEvent"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<DeleteEventMutation, DeleteEventMutationVariables>;
export const MyEventsDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"MyEvents"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"events"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"condition"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"isVisible"},"value":{"kind":"BooleanValue","value":true}},{"kind":"ObjectField","name":{"kind":"Name","value":"isFuture"},"value":{"kind":"BooleanValue","value":true}}]}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"ListValue","values":[{"kind":"EnumValue","value":"SINCE_ASC"}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"MyEvent"}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"Event"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}},{"kind":"Field","name":{"kind":"Name","value":"id"}},{"kind":"Field","name":{"kind":"Name","value":"since"}},{"kind":"Field","name":{"kind":"Name","value":"until"}},{"kind":"Field","name":{"kind":"Name","value":"summary"}},{"kind":"Field","name":{"kind":"Name","value":"description"}},{"kind":"Field","name":{"kind":"Name","value":"filesLegacy"}},{"kind":"Field","name":{"kind":"Name","value":"name"}},{"kind":"Field","name":{"kind":"Name","value":"capacity"}},{"kind":"Field","name":{"kind":"Name","value":"remainingSpots"}},{"kind":"Field","name":{"kind":"Name","value":"locationText"}},{"kind":"Field","name":{"kind":"Name","value":"isLocked"}},{"kind":"Field","name":{"kind":"Name","value":"isVisible"}},{"kind":"Field","name":{"kind":"Name","value":"isPublic"}},{"kind":"Field","name":{"kind":"Name","value":"enableNotes"}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"EventWithItems"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"Event"}},{"kind":"Field","name":{"kind":"Name","value":"attendeeExternals"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"firstName"}},{"kind":"Field","name":{"kind":"Name","value":"lastName"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"attendeeUsers"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"notes"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uNarozeni"}},{"kind":"Field","name":{"kind":"Name","value":"uTelefon"}},{"kind":"Field","name":{"kind":"Name","value":"uEmail"}},{"kind":"Field","name":{"kind":"Name","value":"uRodneCislo"}}]}}]}}]}}]}},{"kind":"FragmentDefinition","name":{"kind":"Name","value":"MyEvent"},"typeCondition":{"kind":"NamedType","name":{"kind":"Name","value":"Event"}},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"FragmentSpread","name":{"kind":"Name","value":"EventWithItems"}}]}}]} as unknown as DocumentNode<MyEventsQuery, MyEventsQueryVariables>;
export const CreateAttendeeExternalDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"CreateAttendeeExternal"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"input"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"CreateParticipationExternalInput"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"createParticipationExternal"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"Variable","name":{"kind":"Name","value":"input"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<CreateAttendeeExternalMutation, CreateAttendeeExternalMutationVariables>;
export const CreateParticipationDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"CreateParticipation"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"input"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"CreateParticipationInput"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"createParticipation"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"Variable","name":{"kind":"Name","value":"input"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<CreateParticipationMutation, CreateParticipationMutationVariables>;
export const CancelParticipationDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"CancelParticipation"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"input"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"CancelParticipationInput"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"cancelParticipation"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"Variable","name":{"kind":"Name","value":"input"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"__typename"}}]}}]}}]} as unknown as DocumentNode<CancelParticipationMutation, CancelParticipationMutationVariables>;