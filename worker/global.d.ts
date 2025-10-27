namespace GraphileWorker {
  interface Tasks {
    csts_scrape_athletes: {
      startFrom?: number;
    };

    notify_announcement: {
      announcement_id: number;
      user_ids: number[];
    };

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
