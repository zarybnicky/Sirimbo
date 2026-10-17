import {
  SetEventRegistrationDocument,
  type EventRegistrationDialogQuery,
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
import { useRegistrationCandidates } from '@/ui/MyRegistrationsDialog';
import { LessonRequestField } from '@/ui/forms/EventRegistrationForm';
import { SubmitButton } from '@/ui/submit';
import { zodResolver } from '@hookform/resolvers/zod';
import { useForm, useWatch } from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';

const personFields = z.object({
  firstName: z.string().overwrite(sanitizeUnicode),
  lastName: z.string().overwrite(sanitizeUnicode),
  gender: z.enum(['MAN', 'WOMAN', 'UNSPECIFIED']),
  email: z.string(),
  phone: z.string(),
  nationality: z.string(),
  birthDate: z.string().nullish(),
  cstsId: z.number().int().positive().nullish(),
  wdsfId: z.number().int().positive().nullish(),
});

const newPerson = personFields.extend({
  firstName: z.string().trim().min(1, 'Zadejte jméno').overwrite(sanitizeUnicode),
  lastName: z.string().trim().min(1, 'Zadejte příjmení').overwrite(sanitizeUnicode),
  email: z.email({ error: 'Zadejte platný e-mail' }),
  phone: z.string().min(9, 'Zadejte telefon').max(14),
  nationality: z.string().min(1, 'Vyberte národnost'),
});

const Form = z
  .object({
    registrant: z.string().min(1, 'Vyberte osobu nebo pár'),
    person: personFields,
    note: z.string(),
    lessonCounts: z.array(z.number().int().min(0)),
  })
  .superRefine((value, ctx) => {
    if (value.registrant !== 'new') return;
    const result = newPerson.safeParse(value.person);
    if (!result.success) {
      for (const issue of result.error.issues) {
        ctx.addIssue({
          code: 'custom',
          path: ['person', ...issue.path],
          message: issue.message,
        });
      }
    }
  });

export default function EventSignupForm({
  instance,
}: {
  instance: NonNullable<EventRegistrationDialogQuery['instance']>;
}) {
  const auth = useAuth();
  const { onSuccess } = useFormResult();
  const [result, register] = useMutation(SetEventRegistrationDocument);

  const { candidates, error, loading } = useRegistrationCandidates(instance, true);
  const options = [...candidates, { id: 'new', label: 'Nová osoba' }];

  const enableDetails = instance.type !== 'LESSON' && instance.type !== 'GROUP';
  const showNotes = enableDetails && instance.enableNotes;
  const trainers = enableDetails
    ? instance.trainersList.filter((t) => t.lessonsOffered !== 0)
    : [];

  const { control, handleSubmit } = useForm({
    resolver: zodResolver(Form),
    defaultValues: {
      registrant: candidates[0]?.id ?? 'new',
      person: {
        firstName: '',
        lastName: '',
        gender: 'UNSPECIFIED' as const,
        email: '',
        phone: '',
        nationality: '203',
      },
      note: '',
      lessonCounts: trainers.map(() => 0),
    },
  });
  const selected = useWatch({ control, name: 'registrant' });

  const save = async (values: z.infer<typeof Form>) => {
    const [kind, id] = values.registrant.split(':');
    const input = {
      pInstanceId: instance.id,
      pIsRegistered: true,
      pPersonId: kind === 'person' ? id : null,
      pCoupleId: kind === 'couple' ? id : null,
      pPerson: kind === 'new' ? newPerson.parse(values.person) : null,
      pNote: showNotes ? values.note : null,
      pLessonTrainerIds: trainers.map((t) => t.id),
      pLessonCounts: values.lessonCounts,
    };
    const response = await register({ input });
    if (!response.error && response.data?.setEventInstanceRegistration) {
      toast.success(
        kind === 'new'
          ? 'Přihláška uložena. Potvrzení a pozvánku vám pošleme e-mailem.'
          : 'Přihlášení proběhlo úspěšně.',
      );
      onSuccess();
    }
  };

  return (
    <form className="grid gap-3" onSubmit={handleSubmit(save)}>
      <FormError error={result.error ?? error} />
      {options.length > 1 && (
        <ComboboxElement
          control={control}
          name="registrant"
          label="Koho přihlásit"
          placeholder="Vyberte osobu nebo pár"
          options={options}
        />
      )}
      {selected === 'new' && (
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
            type="email"
            required
            autoComplete="email"
          />
          <TextFieldElement
            control={control}
            name="person.phone"
            label="Telefon"
            type="tel"
            required
            autoComplete="tel"
          />

          <RadioButtonGroupElement
            control={control}
            name="person.gender"
            label="Pohlaví"
            options={[
              { id: 'MAN', label: 'Muž' },
              { id: 'WOMAN', label: 'Žena' },
              { id: 'UNSPECIFIED', label: 'Neuvedeno' },
            ]}
          />

          <ComboboxElement
            control={control}
            name="person.nationality"
            label="Národnost"
            placeholder="Vyberte národnost"
            options={countryOptions}
          />
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
        </fieldset>
      )}

      {showNotes && (
        <TextAreaElement
          control={control}
          name="note"
          label="Poznámky k registraci, požadavky na stravu apod."
        />
      )}

      {trainers.length > 0 && (
        <fieldset className="grid gap-2">
          <legend>Požadavky na lekce</legend>
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
          loading ||
          result.fetching ||
          (!auth.canManageInstance(instance) &&
            (!!instance.isLocked || instance.registrationInfo?.remainingCapacity === 0))
        }
      >
        Přihlásit
      </SubmitButton>
    </form>
  );
}
