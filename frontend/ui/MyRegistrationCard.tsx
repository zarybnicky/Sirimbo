import {
    CancelRegistrationDocument,
    EventFragment,
    EventRegistrationFragment,
} from '@/graphql/Event';
import { Card } from '@/ui/Card';
import { Dialog, DialogContent, StdDialogTrigger } from '@/ui/dialog';
import { dateTimeFormatter, formatRegistrant } from '@/ui/format';
import { buttonCls } from '@/ui/style';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { MyRegistrationForm } from './forms/MyRegistrationForm';

export type FormProps = {
  note: string;
};

export function MyRegistrationCard({ event, registration }: {
  event: EventFragment;
  registration: EventRegistrationFragment;
}) {
  const cancel = useMutation(CancelRegistrationDocument)[1];
  const onCancel = useAsyncCallback(async () => {
    await cancel({ input: { registrationId: registration.id } });
    toast.success('Přihláška zrušena úspěšně.');
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
          <StdDialogTrigger>Upravit přihlášku</StdDialogTrigger>
          <DialogContent>
            <MyRegistrationForm registration={registration} event={event} />
          </DialogContent>
        </Dialog>
      )}

      <button
        type="button"
        className={buttonCls({ variant: 'outline' })}
        onClick={onCancel.execute}
      >
        Zrušit přihlášku
      </button>
    </Card>
  );
};
