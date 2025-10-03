import { TypedDocumentNode } from '@graphql-typed-document-node/core';
import { parse } from 'graphql';

export interface WebPushChannel {
  id: string;
  provider: string;
  channelIdentifier: string;
  credentials: Record<string, unknown> | null;
  isActive: boolean;
  lastRegisteredAt: string | null;
}

export interface WebPushChannelsQuery {
  pushNotificationChannelsList: WebPushChannel[];
}

export interface WebPushChannelsQueryVariables {
  userId: string;
}

export const WebPushChannelsDocument = parse(/* GraphQL */ `
  query WebPushChannels($userId: BigInt!) {
    pushNotificationChannelsList(
      condition: { userId: $userId, platform: WEB, isActive: true }
    ) {
      id
      provider
      channelIdentifier
      credentials
      isActive
      lastRegisteredAt
    }
  }
`) as TypedDocumentNode<WebPushChannelsQuery, WebPushChannelsQueryVariables>;

export interface UpsertWebPushChannelMutation {
  createPushNotificationChannel: {
    pushNotificationChannel: WebPushChannel | null;
  } | null;
}

export interface UpsertWebPushChannelMutationVariables {
  input: {
    pushNotificationChannel: {
      userId: string;
      platform: 'WEB';
      provider: 'web_push';
      channelIdentifier: string;
      credentials: Record<string, unknown>;
      isActive: boolean;
      lastRegisteredAt: string;
    };
    onConflict: {
      constraint: 'pushNotificationChannelProviderIdentifierKey';
      updateColumns: (
        | 'CREDENTIALS'
        | 'IS_ACTIVE'
        | 'LAST_REGISTERED_AT'
        | 'PLATFORM'
        | 'USER_ID'
      )[];
    };
  };
}

export const UpsertWebPushChannelDocument = parse(/* GraphQL */ `
  mutation UpsertWebPushChannel($input: CreatePushNotificationChannelInput!) {
    createPushNotificationChannel(input: $input) {
      pushNotificationChannel {
        id
        provider
        channelIdentifier
        credentials
        isActive
        lastRegisteredAt
      }
    }
  }
`) as TypedDocumentNode<
  UpsertWebPushChannelMutation,
  UpsertWebPushChannelMutationVariables
>;

export interface DeactivateWebPushChannelMutation {
  updatePushNotificationChannelById: {
    pushNotificationChannel: WebPushChannel | null;
  } | null;
}

export interface DeactivateWebPushChannelMutationVariables {
  input: {
    id: string;
    patch: {
      isActive: boolean;
    };
  };
}

export const DeactivateWebPushChannelDocument = parse(/* GraphQL */ `
  mutation DeactivateWebPushChannel(
    $input: UpdatePushNotificationChannelByIdInput!
  ) {
    updatePushNotificationChannelById(input: $input) {
      pushNotificationChannel {
        id
        provider
        channelIdentifier
        credentials
        isActive
        lastRegisteredAt
      }
    }
  }
`) as TypedDocumentNode<
  DeactivateWebPushChannelMutation,
  DeactivateWebPushChannelMutationVariables
>;
