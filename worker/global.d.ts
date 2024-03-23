namespace GraphileWorker {
  interface Tasks {
    send_invitation: {
      id: string;
    };

    forgotten_password_generate: {
      id: string;
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
