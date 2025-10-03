namespace GraphileWorker {
  interface Tasks {
    send_invitation: {
      id: string;
    };

    forgotten_password_generate: {
      origin: string;
      intent: string;
      users: {
        login: string;
        email: string;
        token: string;
        people: string[];
      }[];
    };

    send_email: {
      template: string;
      options: {
        to: string | string[];
        subject: string;
        html?: string;
        text?: string;
      };
      variables: {
        [varName: string]: any;
      };
    };

    send_push_notification: {
      userId: number;
      notification: {
        title: string;
        body: string;
        icon?: string;
        image?: string;
        clickAction?: string;
      };
      data?: Record<string, unknown>;
      android?: {
        priority?: "normal" | "high";
        channelId?: string;
        ttlSeconds?: number;
        color?: string;
      };
      ios?: {
        sound?: string;
        badge?: number;
        category?: string;
        threadId?: string;
      };
      web?: {
        actions?: Array<{ action: string; title: string; icon?: string }>;
        badge?: string;
        icon?: string;
        image?: string;
        ttlSeconds?: number;
        requireInteraction?: boolean;
      };
    };
  }
}
