import {
  CancelRegistrationDocument,
  type EventFragment,
  type EventRegistrationFragment,
} from '@/graphql/Event';
import { Card } from '@/ui/Card';
import { Dialog, DialogContent, DialogTrigger } from '@/ui/dialog';
import { dateTimeFormatter, formatRegistrant } from '@/ui/format';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { MyRegistrationForm } from '@/ui/forms/MyRegistrationForm';
import { useConfirm } from './Confirm';
import { SubmitButton } from './submit';

export function MyRegistrationCard({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) {
  const confirm = useConfirm();
  const cancel = useMutation(CancelRegistrationDocument)[1];
  const onCancel = useAsyncCallback(async () => {
    await confirm({ description: "Opravdu chcete zrušit přihlášku?" });
    await cancel({ input: { registrationId: registration.id } });
    toast.success('Přihláška zrušena.');
  });

  return (
    <Card className="prose prose-accent">
      <h5>{formatRegistrant(registration)}</h5>
      <div>
        Přihlášeno
        {' v '}
        {dateTimeFormatter.format(new Date(registration.createdAt))}
      </div>
      {registration.eventLessonDemandsByRegistrationIdList.length > 0 && (
        <div>
          <h5>Požadavky na lekce</h5>
          <ul>
            {registration.eventLessonDemandsByRegistrationIdList.map((x) => (
              <li key={x.id}>
                {event.eventTrainersList.find((t) => t.id === x.trainerId)?.name}
                {': '}
                {x.lessonCount}
              </li>
            ))}
          </ul>
        </div>
      )}
      {registration.note && <p>{registration.note}</p>}

      {event.type !== 'LESSON' && (
        <Dialog>
          <DialogTrigger text="Upravit přihlášku" />
          <DialogContent>
            <MyRegistrationForm registration={registration} event={event} />
          </DialogContent>
        </Dialog>
      )}

      <SubmitButton type="button" variant="outline" onClick={onCancel.execute} loading={onCancel.loading}>
        Zrušit přihlášku
      </SubmitButton>
    </Card>
  );
};
