import {
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

interface Props {
  data: MyEventFragment;
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

  const myRegistration = data.attendeeUsers.nodes.find(x => x.user?.uId === user?.id);

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
  const onCancel = useAsyncCallback(async (e?: React.FormEvent) => {
    e?.preventDefault();
    await doCancel({ input: { eventId: data.id! } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextField disabled  label="Člen" value={`${user?.uJmeno} ${user?.uPrijmeni}`} />
      {(data.enableNotes || myRegistration?.notes) ? (
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
        <button className="button button-white" onClick={onCancel.execute}>
          Zrušit přihlášku
        </button>
      )}
    </form>
  );
};
