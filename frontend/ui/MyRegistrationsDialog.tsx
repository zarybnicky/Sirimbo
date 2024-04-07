import { EventFragment, RegisterToEventDocument } from '@/graphql/Event';
import { useAuth } from '@/ui/use-auth';
import * as React from 'react';
import { buttonCls } from '@/ui/style';
import { useFieldArray, useForm } from 'react-hook-form';
import { TextAreaElement } from '@/ui/fields/textarea';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@/ui/form';
import { SubmitButton } from '@/ui/submit';
import { toast } from 'react-toastify';
import { useMutation } from 'urql';
import { CheckCircle, Circle, Plus } from 'lucide-react';
import { formatCoupleName } from '@/ui/format';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from './dialog';
import { MyRegistrationCard } from './MyRegistrationCard';
import { cn } from './cn';
import { NumberFieldElement } from './fields/number';

type FormRegistration = {
  selected: boolean;
  disabled: boolean;
  label: string;
  personId: string | null;
  coupleId: string | null;
  note: string;
  lessons: { trainerId: string; lessonCount: number }[];
};
type FormProps = {
  registrations: FormRegistration[];
};

function NewRegistrationForm({ event, onSuccess }: {
  event: EventFragment;
  onSuccess?: () => void;
}) {
  const create = useMutation(RegisterToEventDocument)[1];
  const { persons, couples } = useAuth();
  const fieldsAddedRef = React.useRef({
    couples: new Set<string | null>(),
    persons: new Set<string | null>(),
  });
  const myRegistrations = event?.myRegistrationsList || [];

  const { watch, register, control, handleSubmit, setValue } = useForm<FormProps>();
  const { fields: fieldsInitial, append } = useFieldArray({ control, name: "registrations" });
  const watchFieldArray = watch("registrations");
  const fields = fieldsInitial.map((field, index) => {
    return {
      ...field,
      ...watchFieldArray[index]
    };
  });

  React.useEffect(() => {
    myRegistrations.forEach(x => {
      fieldsAddedRef.current.couples.add(x.coupleId);
      fieldsAddedRef.current.persons.add(x.personId);
    })

    const newRegistrations: FormRegistration[] = [];
    persons.forEach((p) => {
      if (fieldsAddedRef.current.persons.has(p.id)) return;
      newRegistrations.push({
        personId: p.id,
        coupleId: null,
        selected: false,
        note: '',
        lessons: [],
        label: `${p.firstName} ${p.lastName}`,
        disabled: !myRegistrations.find(r => r.personId === p.id),
      });
      fieldsAddedRef.current.persons.add(p.id);
    });

    if (event.capacity == 0 || (event.remainingPersonSpots ?? 0) > 1) {
      couples.forEach(c => {
        if (fieldsAddedRef.current.couples.has(c.id) || !c.active) return;
        newRegistrations.push({
          coupleId: c.id,
          personId: null,
          selected: false,
          note: '',
          lessons: [],
          label: formatCoupleName(c),
          disabled: !myRegistrations.find(r => r.coupleId === c.id),
        });
        fieldsAddedRef.current.couples.add(c.id);
      });
    }
    if (newRegistrations.length) {
      append(newRegistrations);
    }
  }, [persons, couples, append, event, myRegistrations]);

  const onSubmit = useAsyncCallback(async ({ registrations }: FormProps) => {
    const res = await create({
      input: {
        registrations: registrations.filter(x => x.selected).map(x => ({
          eventId: event.id,
          coupleId: x.coupleId,
          personId: x.personId,
          note: x.note,
          lessons: x.lessons.map((lesson, trainerIdx) => ({
            lessonCount: lesson.lessonCount,
            trainerId: event.eventTrainersList[trainerIdx]!.id,
          })).filter(x => x.lessonCount)
        })),
      },
    });
    const ids = res.data?.registerToEventMany?.eventRegistrations?.map(x => x.id)?.filter(Boolean);
    if (ids && ids.length) {
      toast.success('Přihlášení na akci proběhlo úspěšně.');
      onSuccess?.();
    }
  });

  if (event.capacity > 0 && !event.remainingPersonSpots) {
    return null;
  }

  return (
    <form onSubmit={handleSubmit(onSubmit.execute)} className="space-y-2">
      <FormError error={onSubmit.error} />
      <div className="relative">
      {fields.map(({ id, selected, label }, index) => (
        event.type === "LESSON" ? (
          <button
            type="button"
            key={id}
            data-state={selected ? 'on' : ''}
            onClick={() => setValue(`registrations.${index}.selected`, !selected, {shouldTouch: true})}
            className={cn(
              'flex gap-2 items-center appearance-none',
              'group w-full data-[state=on]:text-white data-[state=on]:bg-accent-9 bg-neutral-1 text-accent-11',
              'px-2.5 py-2 text-sm first:rounded-t-xl border last:rounded-b-xl',
              'border-y border-l last:border-r border-accent-7 data-[state=on]:border-accent-10',
              'disabled:border-neutral-6 disabled:data-[state=on]:border-neutral-10 disabled:data-[state=on]:bg-neutral-9 disabled:text-neutral-11 disabled:data-[state=on]:text-white',
              'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
            )}
          >
            <div className="flex gap-2 items-center">
              {selected ? (
                <CheckCircle className="size-5" />
              ) : (
                <Circle className="size-5" />
              )}
              {label}
            </div>
          </button>
        ) : (
          <div
            key={id}
            className={cn(
              'flex gap-2 items-start',
              'group w-full bg-neutral-1',
              'px-2.5 py-2 text-sm first:rounded-t-xl border last:rounded-b-xl',
              'border-y border-l last:border-r border-accent-7 disabled:border-neutral-6',
              'focus:relative focus:outline-none focus-visible:z-30 focus-visible:ring focus-visible:ring-accent-10',
            )}
          >
            <input
              {...register(`registrations.${index}.selected`)}
              id={`registrations.${index}.selected`}
              type="checkbox"
              className="grow-0 focus:ring-accent-9 size-4 bg-accent-2 text-accent-10 border-accent-9 border-2 rounded"
            />
            <div className="grow flex flex-col gap-2">
              <label className="text-accent-11" htmlFor={`registrations.${index}.selected`}>
                {label}
              </label>
              {selected && event.enableNotes && (
                <TextAreaElement
                  autoFocus
                  control={control}
                  label="Poznámky k registraci, požadavky na stravu apod."
                  name={`registrations.${index}.note`}
                />
              )}
              {selected && (
                <fieldset>
                  <legend className="text-neutral-11">Požadavky na lekce</legend>
                  {event.eventTrainersList.map((trainer, trainerIndex) => (
                    <div key={trainer.id} className="flex items-center flex-wrap gap-2">
                      <div className="grow">
                        {trainer.person?.name}
                      </div>
                      <NumberFieldElement
                        control={control}
                        name={`registrations.${index}.lessons.${trainerIndex}.lessonCount`}
                        min={0}
                        max={trainer.lessonsRemaining || Number.MAX_SAFE_INTEGER}
                      />
                    </div>
                  ))}
                </fieldset>
              )}
            </div>
          </div>
        )
      ))}
      </div>
      <SubmitButton loading={onSubmit.loading}>Přihlásit</SubmitButton>
    </form>
  );
}

export function MyRegistrationsDialog({ event }: { event: EventFragment }) {
  const [open, setOpen] = React.useState(false);

  const myRegistrations = event?.myRegistrationsList || [];

  if (
    event.isLocked ||
    event.eventInstancesList.every(i => new Date(i.since) < new Date()) ||
    (event.capacity > 0 && (event.remainingPersonSpots ?? 0) <= 0 && myRegistrations.length == 0)
  ) {
    return null;
  }

  return (
    <Dialog open={open} onOpenChange={setOpen} modal={false}>
      <DialogTrigger asChild>
        <button className={buttonCls()}>
          {myRegistrations.length > 0 ? (
            <>Moje přihlášky</>
          ) : (
            <><Plus /> Přihlásit</>
          )}
        </button>
      </DialogTrigger>

      <DialogContent>
        <DialogTitle>Moje přihlášky</DialogTitle>

        {myRegistrations.map((reg) => (
          <MyRegistrationCard key={reg.id} event={event} registration={reg} />
        ))}

        {myRegistrations.length > 0 && <div className="text-lg font-bold">Další přihlášky</div>}

        <NewRegistrationForm event={event} onSuccess={() => setOpen(false)} />
      </DialogContent>
    </Dialog>
  );
};
