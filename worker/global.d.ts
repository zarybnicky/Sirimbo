namespace GraphileWorker {
  interface Tasks {
    csts_discover_athletes: {
      lastFoundIdt?: number;
      lastCheckedIdt?: number;
      maxRequests?: number;
    };

    csts_refresh_athletes: {
      maxRequests?: number;
    };

    notify_announcement: {
      announcement_id: number;
      user_ids: number[];
    };

    create_lesson_payments: never;
    refresh_memberships: never;
    refresh_auth_details: never;

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
  }
}
