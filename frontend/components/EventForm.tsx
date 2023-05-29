import { EventInput } from 'lib/graphql';
import {
  CreateEventDocument,
  DeleteEventDocument,
  EventDocument,
  UpdateEventDocument,
} from 'lib/graphql/Event';
import React from 'react';
import { useForm } from 'react-hook-form';
import { TextFieldElement } from 'components/TextField';
import { CheckboxElement } from 'components/Checkbox';
import { useAsyncCallback } from 'react-async-hook';
import { ErrorBox } from './ErrorBox';
import { SubmitButton } from './SubmitButton';
import dynamic from 'next/dynamic';
import { pick } from 'lib/form-utils';
import { pipe } from 'fp-ts/lib/function';
import { useGqlMutation, useGqlQuery } from 'lib/query';
import { useRouter } from 'next/router';
import { Item } from './layout/Item';
import { DeleteButton } from './DeleteButton';
import { Route } from 'nextjs-routes';
const RichTextEditor = dynamic(() => import('./RichTextEditor'), { ssr: false });

const fields = [
  'name',
  'locationText',
  'summary',
  'description',
  'since',
  'until',
  'capacity',
  'isVisible',
  'isPublic',
  'enableNotes',
  'isLocked',
] as const;
type FormProps = Pick<EventInput, (typeof fields)[number]>;

const backHref: Route = { pathname: '/admin/akce' };

export const EventForm: React.FC<{
  id?: string;
}> = ({ id = '' }) => {
  const router = useRouter();
  const query = useGqlQuery(EventDocument, { id }, { enabled: !!id, cacheTime: 0 });
  const data = query.data?.event;

  const onSuccess = React.useCallback(() => {}, []);

  const create = useGqlMutation(CreateEventDocument, { onSuccess });
  const update = useGqlMutation(UpdateEventDocument, { onSuccess });

  const { reset, control, handleSubmit } = useForm<FormProps>();
  React.useEffect(() => {
    if (data) {
      reset(pipe(data, pick(fields)));
    }
  }, [reset, data]);

  const onSubmit = useAsyncCallback(async (patch: FormProps) => {
    if (id) {
      await update.mutateAsync({ id, patch });
    } else {
      await create.mutateAsync({ input: patch });
    }
  });

  return (
    <form
      className="container flex flex-col gap-2"
      onSubmit={handleSubmit(onSubmit.execute)}
    >
      <Item.Titlebar
        backHref={backHref}
        title={id ? data?.name || '(Bez názvu)' : 'Nová akce'}
      >
        {id && (
          <DeleteButton
            doc={DeleteEventDocument}
            id={id}
            title="smazat akci"
            onDelete={() => router.push(backHref)}
          />
        )}
        <SubmitButton loading={onSubmit.loading} />
      </Item.Titlebar>

      <ErrorBox error={onSubmit.error} />
      <TextFieldElement control={control} name="name" label="Název" required />
      <TextFieldElement
        control={control}
        name="locationText"
        label="Místo akce"
        required
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
      <TextFieldElement control={control} type="date" label="Od" name="since" required />
      <TextFieldElement
        type="date"
        helperText="(pokud je prázdné, počítá se jako 'Od')"
        control={control}
        label="Do"
        name="until"
        required
      />
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
      <CheckboxElement
        control={control}
        name="enableNotes"
        value="1"
        label="Povolit poznámky k přihlášce"
      />
      <CheckboxElement control={control} name="isLocked" value="1" label="Uzamčená" />
    </form>
  );
};
