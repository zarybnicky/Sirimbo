import React from 'react';
import { captureException } from '@sentry/nextjs';
import { toast } from 'react-toastify';
import { TypedEventTarget } from 'typescript-event-target';
import type { CombinedError } from 'urql';
import { getErrorMessage } from '@/lib/errors';

export const errorTarget = new TypedEventTarget<{ error: CustomEvent<CombinedError> }>();

const onError = ({ detail: combined }: CustomEvent<CombinedError>) => {
  if (combined.networkError) {
    toast.error(
      `Nastal problém s připojením k serveru (${combined.networkError.message})`,
    );
    return;
  }

  for (const ex of combined.graphQLErrors) {
    const message = getErrorMessage(ex.message);
    toast.error(message ?? ex.message);
    if (!message) {
      captureException(combined);
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
