import {
  type EventRegistrationDialogQuery,
  SetEventRegistrationDocument,
} from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { countryOptions } from '@/lib/countries';
import { sanitizeUnicode } from '@/lib/sanitize';
import { ComboboxElement } from '@/ui/fields/Combobox';
import { DatePickerElement } from '@/ui/fields/date';
import { FederationIdFieldElement } from '@/ui/fields/FederationIdFieldElement';
import { RadioButtonGroupElement } from '@/ui/fields/RadioButtonGroupElement';
import { TextFieldElement } from '@/ui/fields/text';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError, useFormResult } from '@/ui/form';
import { LessonRequestField } from '@/ui/forms/EventRegistrationForm';
import { SubmitButton } from '@/ui/submit';
import { typographyCls } from '@/ui/style';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';

const newPerson = z.object({
  birthDate: z.string().nullish(),
  cstsId: z.number().int().positive().nullish(),
  wdsfId: z.number().int().positive().nullish(),
  gender: z.enum(['MAN', 'WOMAN'], { error: 'Vyberte pohlaví' }),
  firstName: z.string().trim().min(1, 'Zadejte jméno').overwrite(sanitizeUnicode),
  lastName: z.string().trim().min(1, 'Zadejte příjmení').overwrite(sanitizeUnicode),
  email: z.email({ error: 'Zadejte platný e-mail' }),
  phone: z.string().min(9, 'Zadejte telefon').max(14),
  nationality: z.string().min(1, 'Vyberte národnost'),
});

const Form = z.object({
  person: newPerson,
  note: z.string(),
  lessonCounts: z.array(z.number().int().min(0)),
});

export function EventSignupForm({
  event,
}: {
  event: NonNullable<EventRegistrationDialogQuery['event']>;
}) {
  const auth = useAuth();
  const { onSuccess } = useFormResult();
  const [result, register] = useMutation(SetEventRegistrationDocument);

  const enableDetails = event.type !== 'LESSON' && event.type !== 'GROUP';
  const showNotes = enableDetails && event.enableNotes;
  const trainers = enableDetails
    ? event.eventTrainersList.filter((t) => t.lessonsOffered !== 0)
    : [];

  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      person: {
        firstName: '',
        lastName: '',
        email: '',
        phone: '',
        nationality: '203',
      },
      note: '',
      lessonCounts: trainers.map(() => 0),
    },
  });

  const save = async (values: z.infer<typeof Form>) => {
    const input = {
      eventId: event.id,
      isRegistered: true,
      personId: null,
      coupleId: null,
      person: values.person,
      note: showNotes ? values.note : null,
      lessonTrainerIds: trainers.map((t) => t.id),
      lessonCounts: values.lessonCounts,
    };
    const response = await register({ input });
    if (!response.error && response.data?.setEventRegistration) {
      toast.success('Přihláška uložena. Potvrzení a pozvánku vám pošleme e-mailem.');
      onSuccess();
    }
  };

  return (
    <form className="grid gap-3" onSubmit={handleSubmit(save)}>
      <FormError error={result.error} />
      <fieldset className="grid gap-2 sm:grid-cols-2">
        <TextFieldElement
          control={control}
          name="person.firstName"
          label="Jméno"
          required
          autoComplete="given-name"
        />
        <TextFieldElement
          control={control}
          name="person.lastName"
          label="Příjmení"
          required
          autoComplete="family-name"
        />

        <TextFieldElement
          control={control}
          name="person.email"
          label="E-mail"
          placeholder="@"
          type="email"
          required
          autoComplete="email"
        />
        <TextFieldElement
          control={control}
          name="person.phone"
          label="Telefon"
          placeholder="+420..."
          type="tel"
          required
          autoComplete="tel"
        />

        <ComboboxElement
          control={control}
          name="person.nationality"
          label="Národnost"
          placeholder="Vyberte národnost"
          options={countryOptions}
        />
        <RadioButtonGroupElement
          control={control}
          name="person.gender"
          label="Pohlaví"
          options={[
            { id: 'MAN', label: 'Muž' },
            { id: 'WOMAN', label: 'Žena' },
          ]}
        />

        <div className="grid gap-2 sm:col-span-2 sm:grid-cols-3">
          <DatePickerElement
            control={control}
            name="person.birthDate"
            label="Datum narození"
            valueMode="date"
          />
          <FederationIdFieldElement
            control={control}
            name="person.cstsId"
            federation="csts"
            label="ČSTS IDT"
          />
          <FederationIdFieldElement
            control={control}
            name="person.wdsfId"
            federation="wdsf"
            label="WDSF MIN"
          />
        </div>
      </fieldset>
      {showNotes && (
        <TextAreaElement
          control={control}
          name="note"
          label="Poznámky k registraci, požadavky na stravu apod."
          placeholder="Alergie, stravovací omezení, další požadavky…"
        />
      )}

      {trainers.length > 0 && (
        <fieldset className="grid gap-2">
          <legend className={typographyCls({ variant: 'label' })}>
            Požadavky na lekce
          </legend>
          {trainers.map((trainer, i) => (
            <LessonRequestField
              key={trainer.id}
              control={control}
              name={`lessonCounts.${i}`}
              trainer={trainer}
              current={0}
              readOnly={false}
            />
          ))}
        </fieldset>
      )}
      <SubmitButton
        control={control}
        disabled={
          result.fetching ||
          (!auth.canManageEvent(event) &&
            (!!event.isLocked || event.registrationInfo?.remainingCapacity === 0))
        }
      >
        Přihlásit
      </SubmitButton>
    </form>
  );
}
