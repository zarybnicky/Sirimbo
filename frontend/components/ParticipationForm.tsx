import {
  MyEventFragment,
  useCancelParticipationMutation,
  useCreateParticipationMutation,
  useMyEventsQuery,
} from 'lib/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextAreaElement, TextFieldElement } from 'components/TextField';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import { useAuth } from 'lib/data/use-auth';
import { useQueryClient } from '@tanstack/react-query';

type FormProps = {
  myNotes: string;
  yearOfBirth: number;
};

export const ParticipationForm: React.FC<{
  data: MyEventFragment;
  onSuccess: () => void;
}> = ({ data, onSuccess: realOnSuccess }) => {
  const { user } = useAuth();
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    queryClient.invalidateQueries(useMyEventsQuery.getKey());
    realOnSuccess();
  }, [queryClient, realOnSuccess]);

  const { mutateAsync: doUpsert } = useCreateParticipationMutation({ onSuccess });
  const { mutateAsync: doCancel } = useCancelParticipationMutation({ onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      myNotes: data.myNotes || '',
      yearOfBirth: new Date(user?.uNarozeni || '').getFullYear(),
    });
  }, [reset, user, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await doUpsert({ input: { eventId: data.id!, ...values } });
  });
  const onCancel = useAsyncCallback(async (e?: React.FormEvent) => {
    e?.preventDefault();
    await doCancel({ input: { eventId: data.id! } });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <ErrorBox error={onSubmit.error} />
      <TextFieldElement
        disabled
        control={control}
        label="Člen"
        name="yearOfBirth"
        value={`${user?.uJmeno} ${user?.uPrijmeni}`}
      />
      <TextFieldElement
        disabled
        control={control}
        label="Rok narození"
        name="yearOfBirth"
      />
      {data.enableNotes || data.myNotes ? (
        <TextAreaElement
          autoFocus
          control={control}
          label="Požadavky na lekce, stravu apod."
          name="myNotes"
        />
      ) : null}
      <SubmitButton loading={onSubmit.loading}>
        {data.signedUp ? 'Upravit přihlášku' : 'Přihlásit'}
      </SubmitButton>
      {data?.signedUp && (
        <button className="button button-white" onClick={onCancel.execute}>
          Zrušit přihlášku
        </button>
      )}
    </form>
  );
};
