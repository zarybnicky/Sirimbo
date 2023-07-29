import {
  CancelParticipationDocument,
  CreateAttendeeExternalDocument,
  CreateParticipationDocument,
  EventFragment,
  MyEventFragment,
} from '@app/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextField, TextFieldElement } from '@app/ui/fields/text';
import { TextAreaElement } from '@app/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useAuth } from '@app/ui/use-auth';
import { toast } from 'react-toastify';
import { Dialog, DialogContent, DialogTrigger } from '@app/ui/dialog';
import { useMutation } from 'urql';
import { z } from 'zod';
import { zodResolver } from '@hookform/resolvers/zod';
import { buttonCls } from './style/button';

interface Props {
  data: EventFragment & Partial<MyEventFragment>;
  onSuccess: () => void;
}

const ExternalForm = z.object({
  firstName: z.string(),
  lastName: z.string(),
  guardianName: z.string().optional(),
  email: z.string().email(),
  phone: z.string(),
  notes: z.string().optional(),
  birthNumber: z.string().regex(/[0-9]{9,10}/, 'Neplatné rodné číslo'),
});
type ExternalFormProps = z.infer<typeof ExternalForm>;

type FormProps = {
  myNotes: string;
};

function ExternalParticipationForm({ data, onSuccess }: Props) {
  const create = useMutation(CreateAttendeeExternalDocument)[1];
  const { control, handleSubmit } = useForm<ExternalFormProps>({ resolver: zodResolver(ExternalForm) });

  const onSubmit = useAsyncCallback(async (values: ExternalFormProps) => {
    await create({
      input: {
        ...values,
        birthNumber: values.birthNumber || '',
        guardianName: values.guardianName || '',
        notes: values.notes || '',
        eventId: data.id,
      },
    });
    toast.success('Registrace proběhla úspěšně.');
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <div>Přihlášení na akci</div>
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
      />

      <div className="mt-2">Kontaktní údaje</div>
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

function ParticipationForm({ data, onSuccess }: Props) {
  const { user } = useAuth();
  const myRegistration = data.attendeeUsers?.nodes?.find((x) => x.user?.uId === user?.id);

  const create = useMutation(CreateParticipationDocument)[1];
  const cancel = useMutation(CancelParticipationDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    reset({
      myNotes: myRegistration?.notes || '',
    });
  }, [reset, user, myRegistration]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    await create({input: { eventId: data.id!, ...values }});
    toast.success('Registrace proběhla úspěšně.');
    onSuccess?.();
  });
  const onCancel = useAsyncCallback(async () => {
    await cancel({ input: { eventId: data.id! } });
    toast.success('Registrace zrušena úspěšně.');
    onSuccess?.();
  });

  return (
    <form className="grid gap-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <FormError error={onSubmit.error} />
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
        <button type="button" className={buttonCls({ variant: 'outline' })} onClick={onCancel.execute}>
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
  const [open, setOpen] = React.useState(false);
  const close = React.useCallback(() => setOpen(false), []);
  const { user } = useAuth();
  const myRegistration = data.attendeeUsers?.nodes?.find((x) => x.user?.uId === user?.id);

  if (data.isLocked || (!myRegistration && (data.remainingSpots ?? 0) < 1)) {
    return null;
  }
  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger className={buttonCls()}>
        {myRegistration ? 'Upravit přihlášku' : 'Přihlásit'}
      </DialogTrigger>
      <DialogContent>
        {user ? (
          <ParticipationForm data={data} onSuccess={close} />
        ) : (
          <ExternalParticipationForm data={data} onSuccess={close} />
        )}
      </DialogContent>
    </Dialog>
  );
};
