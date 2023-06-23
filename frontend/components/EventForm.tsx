import { EventInput } from '@app/graphql';
import {
  CreateEventDocument,
  DeleteEventDocument,
  EventDocument,
  UpdateEventDocument,
} from '@app/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from '@app/ui/fields/text';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { FormError } from '@app/ui/form';
import { SubmitButton } from '@app/ui/submit';
import { useRouter } from 'next/router';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
import { DateRange, DateRangeInput } from '@app/ui/fields/date';
import { ErrorPage } from './ErrorPage';
import { toast } from 'react-toastify';
import { useMutation, useQuery } from 'urql';
import { RichTextEditor } from './RichTextEditor';
import { TitleBar } from './layout/TitleBar';

type FormProps = Pick<
  EventInput,
  | 'name'
  | 'locationText'
  | 'summary'
  | 'description'
  | 'descriptionMember'
  | 'capacity'
  | 'isVisible'
  | 'isPublic'
  | 'enableNotes'
  | 'isLocked'
> & {
  schedule: DateRange;
};

const backHref: Route = { pathname: '/admin/akce' };

export const EventForm = ({ id = '' }: { id?: string }) => {
  const router = useRouter();
  const [query] = useQuery({query: EventDocument, variables: { id }, pause: !id });
  const data = query.data?.event;
  const title = id ? data?.name || '(Bez názvu)' : 'Nová akce';

  const create = useMutation(CreateEventDocument)[1];
  const update = useMutation(UpdateEventDocument)[1];

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (data) {
      reset({
        name: data?.name,
        locationText: data?.locationText,
        summary: data?.summary,
        description: data?.description,
        capacity: data?.capacity,
        isVisible: data?.isVisible,
        isPublic: data?.isPublic,
        isLocked: data?.isLocked,
        enableNotes: data?.enableNotes,
        schedule: {
          from: data?.since ? new Date(data?.since) : undefined,
          to: data?.until ? new Date(data?.until) : undefined,
        },
      });
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (values: FormProps) => {
    const patch = {
      name: values.name,
      locationText: values.locationText,
      summary: values.summary,
      description: values.description,
      capacity: values.capacity,
      isVisible: values.isVisible,
      isPublic: values.isPublic,
      isLocked: values.isLocked,
      enableNotes: values.enableNotes,
      since: values.schedule.from?.toISOString() || '',
      until: values.schedule.to?.toDateString() || '',
    };
    if (id) {
      await update({ id, patch });
    } else {
      const res = await create({ input: patch });
      const id = res.data!.createEvent?.event?.id;
      toast.success('Přidáno.');
      if (id) {
        router.replace({ pathname: '/admin/nabidka/[id]', query: { id } });
      } else {
        reset(undefined);
      }
    }
  });

  if (query.data && query.data.event === null) {
    return <ErrorPage error="Nenalezeno" />;
  }

  return (
    <form className="container space-y-2" onSubmit={handleSubmit(onSubmit.execute)}>
      <TitleBar backHref={backHref} title={title} >
        {id && (
          <DeleteButton
            doc={DeleteEventDocument}
            id={id}
            title="smazat akci"
            redirect={backHref}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </TitleBar>

      <FormError error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement
        control={control}
        name="locationText"
        label="Místo akce"
        required
      />
      <DateRangeInput control={control} name="schedule" label="Datum" />
      <TextFieldElement
        control={control}
        type="number"
        name="capacity"
        label="Kapacita"
        required
      />
      <CheckboxElement
        control={control}
        name="isVisible"
        value="1"
        label="Zviditelnit pro členy"
      />
      <CheckboxElement
        control={control}
        name="isPublic"
        value="1"
        label="Zviditelnit pro veřejnost"
      />
      <CheckboxElement control={control} name="isLocked" value="1" label="Uzamčená" />
      <CheckboxElement
        control={control}
        name="enableNotes"
        value="1"
        label="Povolit poznámky k přihlášce"
      />
      <RichTextEditor
        control={control}
        initialState={data?.summary}
        name="summary"
        label="Shrnutí"
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="description"
        label="Další info"
      />
      <RichTextEditor
        control={control}
        initialState={data?.description}
        name="descriptionMember"
        label="Další info pro členy"
      />
    </form>
  );
};
