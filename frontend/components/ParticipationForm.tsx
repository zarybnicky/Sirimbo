import {
  EventFragment,
  MyEventFragment,
  useCancelParticipationMutation,
  useCreateAttendeeExternalMutation,
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
import type { AttendeeExternalInput } from 'lib/graphql';
import { toast } from 'react-toastify';

interface Props {
  data: EventFragment & Partial<MyEventFragment>;
  onSuccess: () => void;
}

type FormProps = {
  myNotes: string;
};

type ExternalFormProps = Pick<
  AttendeeExternalInput,
  'firstName' | 'lastName' | 'guardianName' | 'email' | 'phone' | 'notes' | 'birthNumber'
>;

function ExternalParticipationForm({ data, onSuccess: realOnSuccess }: Props) {
  const queryClient = useQueryClient();
  const onSuccess = React.useCallback(() => {
    toast.success('Registrace proběhla úspěšně.');
    queryClient.invalidateQueries(useMyEventsQuery.getKey());
    realOnSuccess();
  }, [queryClient, realOnSuccess]);

  const { mutateAsync: doCreate } = useCreateAttendeeExternalMutation({ onSuccess });
  const { control, handleSubmit } = useForm<ExternalFormProps>();

  const onSubmit = useAsyncCallback(async (values: ExternalFormProps) => {
    await doCreate({
      input: {
        ...values,
        birthNumber: values.birthNumber || '',
        guardianName: values.guardianName || '',
        notes: values.notes || '',
        eventId: data.id,
      },
    });
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <div className="h3">Přihlášení na akci</div>
      <div className="flex flex-wrap gap-1 w-full">
        <TextFieldElement
          className="grow"
          control={control}
          name="firstName"
          label="Jméno účastníka"
          required
        />
        <TextFieldElement
          className="grow"
          control={control}
          name="lastName"
          label="Příjmení účastníka"
          required
        />
      </div>
      <TextFieldElement
        control={control}
        name="birthNumber"
        label="Rodné číslo účastníka"
        required
        placeholder="1111119999"
        validation={{
          pattern: {
            value: /[0-9]{9,10}/,
            message: 'Neplatné rodné číslo',
          },
        }}
      />

      <div className="h3 mt-2">Kontaktní údaje</div>
      <TextFieldElement
        control={control}
        name="guardianName"
        label="Jméno zákonného zástupce (pro mladší 18 let)"
        defaultValue=""
      />
      <div className="flex flex-wrap gap-1 w-full mb-2">
        <TextFieldElement
          className="grow"
          control={control}
          name="email"
          type="email"
          placeholder="@"
          label="E-mail"
          required
        />
        <TextFieldElement
          className="grow"
          control={control}
          name="phone"
          type="tel"
          placeholder="+420"
          label="Telefon"
          required
        />
      </div>
      {data.enableNotes && (
        <TextAreaElement
          control={control}
          name="notes"
          label="Požadavky na lekce, stravu, ..."
        />
      )}
      <SubmitButton loading={onSubmit.loading}>Přihlásit</SubmitButton>
    </form>
  );
}

function ParticipationForm({ data, onSuccess: realOnSuccess }: Props) {
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
}

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
    <SimpleDialog button={button}>
      {({ close }) =>
        user ? (
          <ParticipationForm data={data} onSuccess={close} />
        ) : (
          <ExternalParticipationForm data={data} onSuccess={close} />
        )
      }
    </SimpleDialog>
  );
};
