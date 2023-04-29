import {
  EventFragment,
  MyEventFragment,
  useCancelParticipationMutation,
  useCreateParticipationMutation,
  useMyEventsQuery,
} from 'lib/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextField, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { useAuth } from 'lib/data/use-auth';
import { useQueryClient } from '@tanstack/react-query';
import { SimpleDialog } from './Dialog';
import { Button } from './Button';

interface Props {
  data: EventFragment & Partial<MyEventFragment>;
  onSuccess: () => void;
}

type FormProps = {
  myNotes: string;
};

export const ParticipationForm = ({ data, onSuccess: realOnSuccess }: Props) => {
  const { user } = useAuth();
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useMyEventsQuery.getKey());
    realOnSuccess();
  }, [queryClient, realOnSuccess]);

  const myRegistration = data.attendeeUsers?.nodes?.find((x) => x.user?.uId === user?.id);

  const { mutateAsync: doUpsert } = useCreateParticipationMutation({ onSuccess });
  const { mutateAsync: doCancel } = useCancelParticipationMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      myNotes: myRegistration?.notes || '',
    });
  }, [reset, user, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpsert({ input: { eventId: data.id!, ...values, yearOfBirth: 0 } });
  });
  const onCancel = useAsyncCallback(async () => {
    await doCancel({ input: { eventId: data.id! } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextField disabled label="Člen" value={`${user?.uJmeno} ${user?.uPrijmeni}`} />
      {data.enableNotes || myRegistration?.notes ? (
        <TextAreaElement
          autoFocus
          control={control}
          label="Požadavky na lekce, stravu apod."
          name="myNotes"
        />
      ) : null}
      <SubmitButton loading={onSubmit.loading}>
        {myRegistration ? 'Upravit přihlášku' : 'Přihlásit'}
      </SubmitButton>
      {myRegistration && (
        <button type="button" className="button button-white" onClick={onCancel.execute}>
          Zrušit přihlášku
        </button>
      )}
    </form>
  );
};

interface DialogProps {
  data: EventFragment & Partial<MyEventFragment>;
}

export const ParticipationDialog = ({ data }: DialogProps) => {
  const { user } = useAuth();
  const myRegistration = data.attendeeUsers?.nodes?.find((x) => x.user?.uId === user?.id);

  if (data.isLocked || (!myRegistration && (data.remainingSpots ?? 0) < 1)) {
    return null;
  }
  const button = <Button>{myRegistration ? 'Upravit přihlášku' : 'Přihlásit'}</Button>;
  return (
    <SimpleDialog title="Přihlásit na akci" button={button}>
      {({ close }) => <ParticipationForm data={data} onSuccess={close} />}
    </SimpleDialog>
  );
};
