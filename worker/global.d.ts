namespace GraphileWorker {
  interface Tasks {
    frontier_fetch: { id: string };
    frontier_process: { id: string };
    frontier_schedule: {};
    frontier_validate: { federation: string; kind: string; };
    discover_csts_athletes: {};

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
