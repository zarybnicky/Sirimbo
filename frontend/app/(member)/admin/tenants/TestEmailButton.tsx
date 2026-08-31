'use client';

import { Mail } from 'lucide-react';
import { useAsyncCallback } from 'react-async-hook';
import { SubmitButton } from '@/ui/submit';
import { sendTestEmail } from './actions';

export function TestEmailButton() {
  const sendEmail = useAsyncCallback(sendTestEmail);

  return (
    <SubmitButton action={sendEmail}>
      <Mail />
      Poslat testovací e-mail
    </SubmitButton>
  );
}
