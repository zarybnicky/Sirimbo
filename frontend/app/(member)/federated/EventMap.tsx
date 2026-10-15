'use client';

import React from 'react';
import Link from 'next/link';
import * as Plot from '@observablehq/plot';
import { json, type ExtendedFeatureCollection } from 'd3';
import { parseAsInteger, parseAsStringLiteral, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import {
  FederationEventFacetsDocument,
  FederationMapEventsDocument,
  type FederationMapEventFragment,
} from '@/graphql/FederationMap';
import { SelectField } from '@/ui/fields/select';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { Table } from '@/ui/Table';
import { FormError } from '@/ui/form';
import { formatDateOnly } from '@/ui/format';

const shapes = [
  { value: 'czechia', label: 'Česko' },
  { value: 'europe', label: 'Evropa' },
  { value: 'world', label: 'Svět' },
] as const;
type MapShape = (typeof shapes)[number]['value'];
const defaultShapes: Record<string, MapShape> = { csts: 'czechia', szts: 'europe', wdsf: 'world' };

export default function EventMap({ federation }: { federation: string }) {
  const [year, setYear] = useQueryState(
    'year',
    parseAsInteger
      .withDefault(new Date().getFullYear())
      .withOptions({ history: 'push', scroll: false }),
  );
  const [shape, setShape] = useQueryState(
    'shape',
    parseAsStringLiteral(shapes.map((s) => s.value))
      .withDefault(defaultShapes[federation] ?? 'world')
      .withOptions({ history: 'push', scroll: false }),
  );
  const [facets] = useQuery({
    query: FederationEventFacetsDocument,
    variables: { federation },
  });
  const years = (facets.data?.federation?.eventFacets?.years ?? []).filter(
    (y) => y !== null,
  );
  const [result] = useQuery({
    query: FederationMapEventsDocument,
    variables: {
      federation,
      date: { since: `${year}-01-01`, until: `${year + 1}-01-01` },
    },
  });
  const events = result.data?.federation?.events.nodes ?? [];
  const missing = events.filter((e) => e.venueLat === null || e.venueLng === null);
  const error = facets.error || result.error;
  if (error) return <FormError error={error} />;

  return (
    <div className="min-w-0">
      <div className="mb-4 flex flex-wrap items-end gap-3">
        <SelectField name="shape" label="Mapa" value={shape} onChange={setShape} options={shapes} />
        <SelectField
          name="year"
          label="Rok"
          value={String(year)}
          onChange={(value) => void setYear(Number(value))}
          disabled={years.length === 0}
          options={years.map((y) => ({
            value: String(y.year),
            label: `${y.year} · ${y.eventCount} událostí (${y.mappedEventCount} na mapě)`,
          }))}
        />
        {missing.length > 0 && (
          <Dialog key={year}>
            <DialogTrigger size="sm" text={`Bez souřadnic (${missing.length})`} />
            <DialogContent className="sm:max-w-3xl">
              <DialogTitle>Events without coordinates · {year}</DialogTitle>
              <Table columns={['Date', 'Event', 'Location', 'Country']}>
                {missing.map((event) => (
                  <tr key={event.id} className="border-t border-neutral-4">
                    <td className="whitespace-nowrap">
                      {formatDateOnly(event.startDate)}
                    </td>
                    <th scope="row">
                      <Link
                        className="text-accent-11 underline"
                        href={`/federated/${federation}/${event.externalId}`}
                      >
                        {event.name || event.externalId}
                      </Link>
                    </th>
                    <td>
                      <div>{event.city || event.location || '–'}</div>
                      <div>
                        {[event.streetAddress, event.postalCode]
                          .filter(Boolean)
                          .join(', ')}
                      </div>
                    </td>
                    <td>{event.country || '–'}</td>
                  </tr>
                ))}
              </Table>
            </DialogContent>
          </Dialog>
        )}
      </div>
      <MapPlot key={shape} events={events} shape={shape} />
    </div>
  );
}

function MapPlot({ events, shape }: { events: FederationMapEventFragment[]; shape: MapShape }) {
  const [geography, setGeography] = React.useState<ExtendedFeatureCollection>();
  const [error, setError] = React.useState<Error>();
  const containerRef = React.useRef<HTMLDivElement | null>(null);
  React.useEffect(() => {
    const controller = new AbortController();
    json<ExtendedFeatureCollection>(shape === 'czechia' ? '/cz-admin1.json' : '/world-countries.json', {
      signal: controller.signal,
    }).then(setGeography).catch((error) => {
      if (!controller.signal.aborted) setError(error);
    });
    return () => controller.abort();
  }, [shape]);
  React.useEffect(() => {
    if (!geography || !containerRef.current) return;
    const months = [
      'leden',
      'únor',
      'březen',
      'duben',
      'květen',
      'červen',
      'červenec',
      'srpen',
      'září',
      'říjen',
      'listopad',
      'prosinec',
    ];
    const projections: Record<MapShape, Plot.ProjectionOptions> = {
      czechia: {
        type: 'azimuthal-equidistant',
        domain: geography,
        rotate: [-20, 0],
      },
      europe: {
        type: 'mercator',
        domain: { type: 'MultiPoint', coordinates: [[-25, 34], [45, 72]] },
      },
      world: { type: 'equal-earth' },
    };
    const plot = Plot.plot({
      height: 400,
      projection: { ...projections[shape], inset: 5 },
      axis: null,
      color: { type: 'categorical', scheme: 'sinebow', domain: months, legend: true },
      style: { fontFamily: 'Georgia' },
      marks: [
        Plot.geo(geography, { stroke: '#aaa' }),
        Plot.dot(
          events.filter((e) => e.venueLat !== null && e.venueLng !== null),
          {
            y: 'venueLat',
            x: 'venueLng',
            fill: (x) => months[Number(x.startDate.slice(5, 7)) - 1],
            r: 2,
            channels: {
              title: (x) => [formatDateOnly(x.startDate), x.name, x.location].join('\n'),
            },
            tip: true,
          },
        ),
      ],
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [geography, events, shape]);
  return error ? <FormError error={error} /> : <div ref={containerRef} />;
}
