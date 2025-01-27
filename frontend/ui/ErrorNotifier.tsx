import React from 'react';
import * as Sentry from '@sentry/nextjs';
import { toast } from 'react-toastify';
import { TypedEventTarget } from 'typescript-event-target';
import type { CombinedError } from 'urql';

export const errorTarget = new TypedEventTarget<{ error: CustomEvent<CombinedError> }>()

const onError = ({ detail: combined }: CustomEvent<CombinedError>) => {
  if (combined.networkError) {
    toast.error(`Nastal problém s připojením k serveru (${combined.networkError.message})`);
    return;
  }

  for (const ex of combined.graphQLErrors) {
    if (ex.message === 'INVALID_CREDENTIALS') {
      toast.error('Neplatné přihlašovací údaje');
    } else if (ex.message === 'duplicate key value violates unique constraint "users_email_key"') {
      toast.error('Zřejmě již v systému máte účet. Přihlašte se a vyplňte si přihlášku v sekci "Profil"');
    } else {
      toast.error(ex.message);
      Sentry.captureException(combined);
    }
  }
};

export function ErrorNotifier() {
  React.useEffect(() => {
    errorTarget.addEventListener('error', onError);
    return () => errorTarget.removeEventListener('error', onError);
  }, []);
  return null;
}
