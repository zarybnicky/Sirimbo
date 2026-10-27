import {
  type EventRegistrationFragment,
  type EventTrainerFragment,
  type EventRegistrationDialogQuery,
  SetEventRegistrationDocument,
} from '@/graphql/Event';
import { useAuth } from '@/lib/auth';
import { TextAreaElement } from '@/ui/fields/textarea';
import { FormError } from '@/ui/form';
import { buttonCls, typographyCls } from '@/ui/style';
import { SubmitButton } from '@/ui/submit';
import { useConfirm } from '@/ui/Confirm';
import { zodResolver } from '@hookform/resolvers/zod';
import { Minus, Plus } from 'lucide-react';
import {
  type Control,
  type FieldPathByValue,
  type FieldValues,
  useController,
  useForm,
} from 'react-hook-form';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { z } from 'zod';
import { useAsyncCallback } from 'react-async-hook';

const Form = z.object({
  note: z.string(),
  lessonCounts: z.array(z.number().min(0)),
});

type FormValues = z.infer<typeof Form>;

export function EventRegistrationForm({
  event,
  registrant,
  onSaved,
  onCancelled,
}: {
  event: NonNullable<EventRegistrationDialogQuery['event']>;
  registrant:
    | EventRegistrationFragment
    | Pick<EventRegistrationFragment, 'personId' | 'coupleId'>;
  onSaved: () => void;
  onCancelled: () => void;
}) {
  const auth = useAuth();
  const confirm = useConfirm();
  const [result, setRegistration] = useMutation(SetEventRegistrationDocument);
  const registration = 'requests' in registrant ? registrant : undefined;
  const { personId, coupleId } = registrant;
  const enableDetails = event.type !== 'LESSON' && event.type !== 'GROUP';
  const readOnly = !!event.isLocked && !auth.canManageEvent(event);
  const requestCounts = Object.fromEntries(
    registration?.requests.map((x) => [x.trainerId, x.lessonCount]) ?? [],
  );
  const lessonTrainers = enableDetails
    ? event.trainersList.filter(
        (x) => x.lessonsOffered !== 0 || (requestCounts[x.id] ?? 0) > 0,
      )
    : [];
  const showNotes = enableDetails && (event.enableNotes || !!registration?.note);
  const hasFields = showNotes || lessonTrainers.length > 0;
  const { control, handleSubmit } = useForm<FormValues>({
    resolver: zodResolver(Form),
    defaultValues: {
      note: registration?.note ?? '',
      lessonCounts: lessonTrainers.map((x) => requestCounts[x.id] ?? 0),
    },
  });

  const save = async (values: FormValues) => {
    const result = await setRegistration({
      input: {
        eventId: event.id,
        personId,
        coupleId,
        isRegistered: true,
        note: showNotes ? values.note : null,
        lessonTrainerIds: lessonTrainers.map((trainer) => trainer.id),
        lessonCounts: lessonTrainers.map((_, index) => values.lessonCounts[index] ?? 0),
      },
    });
    if (!result.error) {
      toast.success(
        registration ? 'Přihláška upravena.' : 'Přihlášení proběhlo úspěšně.',
      );
      onSaved();
    }
  };

  const cancel = useAsyncCallback(async () => {
    try {
      await confirm({ description: 'Opravdu chcete zrušit přihlášku?' });
    } catch {
      return;
    }
    const result = await setRegistration({
      input: {
        eventId: event.id,
        personId,
        coupleId,
        isRegistered: false,
      },
    });
    if (!result.error) {
      toast.success('Přihláška zrušena.');
      onCancelled();
    }
  });

  return (
    <form className="grid gap-3" onSubmit={handleSubmit(save)}>
      <FormError error={result.error} />

      {showNotes && (
        <TextAreaElement
          control={control}
          disabled={readOnly}
          label="Poznámky k registraci, požadavky na stravu apod."
          placeholder="Alergie, stravovací omezení, další požadavky…"
          name="note"
        />
      )}

      {lessonTrainers.length > 0 && (
        <fieldset className="grid gap-2">
          <legend className={typographyCls({ variant: 'label' })}>
            Požadavky na lekce
          </legend>
          {lessonTrainers.map((trainer, index) => (
            <LessonRequestField
              key={trainer.id}
              control={control}
              name={`lessonCounts.${index}`}
              trainer={trainer}
              current={requestCounts[trainer.id] ?? 0}
              readOnly={readOnly}
            />
          ))}
        </fieldset>
      )}

      {readOnly ? (
        <div className="text-sm text-neutral-11">Přihlášky jsou uzamčené.</div>
      ) : (
        <div className="flex justify-between gap-2">
          {registration && (
            <SubmitButton variant="outline" action={cancel}>
              Zrušit přihlášku
            </SubmitButton>
          )}
          {(!registration || hasFields) && (
            <SubmitButton
              className="ml-auto"
              control={control}
              disabled={result.fetching}
            >
              {registration ? 'Uložit' : 'Přihlásit'}
            </SubmitButton>
          )}
        </div>
      )}
    </form>
  );
}

export function LessonRequestField<T extends FieldValues>({
  control,
  name,
  trainer,
  current,
  readOnly,
}: {
  control: Control<T>;
  name: FieldPathByValue<T, number>;
  trainer: EventTrainerFragment;
  current: number;
  readOnly: boolean;
}) {
  const { field } = useController({ control, name });
  const value = typeof field.value === 'number' ? field.value : 0;
  const label = trainer.person?.name;
  const max =
    trainer.lessonsOffered === null
      ? null
      : current + Math.max(trainer.lessonsRemaining ?? 0, 0);

  return (
    <div className="flex flex-wrap items-center gap-2">
      <span className="grow">{label}</span>
      <div className="ml-auto flex items-center gap-2">
        {max !== null && (
          <span className="text-sm text-neutral-10">zbývá {max - value}</span>
        )}
        <button
          type="button"
          className={buttonCls({ size: 'icon', variant: 'outline' })}
          aria-label={`Snížit počet lekcí s ${label}`}
          disabled={readOnly || value === 0}
          onClick={() => field.onChange(value - 1)}
        >
          <Minus />
        </button>
        <output className="px-1 tabular-nums">{value}</output>
        <button
          type="button"
          className={buttonCls({ size: 'icon', variant: 'outline' })}
          aria-label={`Zvýšit počet lekcí s ${label}`}
          disabled={readOnly || (max !== null && value >= max)}
          onClick={() => field.onChange(value + 1)}
        >
          <Plus />
        </button>
      </div>
    </div>
  );
}
