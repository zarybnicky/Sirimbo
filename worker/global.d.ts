namespace GraphileWorker {
  interface Tasks {
    frontier_fetch: { id: string };
    frontier_process: { isFullRebuild?: boolean };
    frontier_schedule: unknown;
    discover_csts_athletes: unknown;

    notify_announcement: {
      announcement_id: number;
      user_ids: number[];
    };

    create_lesson_payments: never;
    refresh_memberships: never;
    refresh_account_balances: never;

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
