'use client';

import React from 'react';
import Link from 'next/link';
import {
  geoAzimuthalEquidistant,
  geoGraticule10,
  geoPath,
  groups,
  interpolateSinebow,
  json,
  select,
  zoom,
  zoomIdentity,
  type ExtendedFeatureCollection,
} from 'd3';
import { Minus, Plus, RotateCcw } from 'lucide-react';
import { buttonCls } from '@/ui/style';
import { geoRobinson } from 'd3-geo-projection';
import type { FederationMapEventFragment } from '@/graphql/Federation';
import { FormError } from '@/ui/form';
import { SelectField } from '@/ui/fields/select';
import { formatCount, formatDateOnly, monthNameFormatter } from '@/ui/format';
import { Popover, PopoverContent, PopoverAnchor } from '@/ui/popover';
import { cn } from '@/lib/cn';

export const eventMonths = Array.from({ length: 12 }, (_, month) =>
  monthNameFormatter.format(new Date(2000, month, 1)),
);

export const eventMonthColors = eventMonths.map((_, i) =>
  interpolateSinebow(i / eventMonths.length),
);

export const mapViews = [
  { value: 'czechia', label: 'Česko' },
  { value: 'europe', label: 'Evropa' },
  { value: 'world', label: 'Svět' },
] as const;
type MapView = (typeof mapViews)[number]['value'];
export const defaultMapViews: Record<string, MapView> = {
  csts: 'czechia',
  szts: 'europe',
  wdsf: 'world',
};

const graticule = geoGraticule10();
const europe = {
  type: 'MultiPoint' as const,
  coordinates: [
    [-25, 34],
    [-25, 72],
    [45, 34],
    [45, 72],
  ],
};

export default function EventMap({
  events,
  view,
  onViewChange,
  federation,
}: {
  events: FederationMapEventFragment[];
  view: MapView;
  onViewChange: (view: MapView) => void;
  federation: string;
}) {
  const locations = React.useMemo(() => eventLocations(events), [events]);
  const [selected, setSelected] = React.useState<{
    key: string;
    anchor: HTMLButtonElement;
    pinned: boolean;
  } | null>(null);
  const containerRef = React.useRef<HTMLDivElement>(null);
  const [geography, setGeography] = React.useState<ExtendedFeatureCollection>();
  const [error, setError] = React.useState<Error>();
  const [map, setMap] = React.useState<ReturnType<typeof projectMap>>();
  const [transform, setTransform] = React.useState(zoomIdentity);
  const czechia = view === 'czechia';
  const behavior = React.useMemo(
    () =>
      zoom<HTMLDivElement, unknown>()
        .scaleExtent([1, 8])
        .filter((event) => !event.button && (event.type !== 'wheel' || event.ctrlKey)),
    [],
  );

  React.useEffect(() => {
    const controller = new AbortController();
    json<ExtendedFeatureCollection>(
      czechia ? '/cz-admin1.json' : '/world-countries.json',
      { signal: controller.signal },
    )
      .then(setGeography)
      .catch((error) => {
        if (!controller.signal.aborted) setError(error);
      });
    return () => controller.abort();
  }, [czechia]);
  React.useEffect(() => {
    if (!geography) return;
    const element = containerRef.current!;
    const container = select(element).call(
      behavior.on('zoom', (event) => {
        setTransform(event.transform);
        setSelected(null);
      }),
    );
    const observer = new ResizeObserver(() => {
      const { clientWidth: width, clientHeight: height } = element;
      if (!width || !height) return;
      const map = projectMap(geography, view, width, height);
      behavior.translateExtent([
        [0, 0],
        [width, height],
      ]);
      setMap(map);
      container.call(behavior.transform, map.initial);
    });
    observer.observe(element);
    return () => {
      observer.disconnect();
      container.on('.zoom', null);
    };
  }, [geography, view, behavior]);

  const closeTimer = React.useRef<ReturnType<typeof setTimeout>>(undefined);
  React.useEffect(() => () => clearTimeout(closeTimer.current), []);

  function open(key: string, anchor: HTMLButtonElement, pinned = false) {
    clearTimeout(closeTimer.current);
    setSelected((current) =>
      current?.pinned && !pinned ? current : { key, anchor, pinned },
    );
  }
  function closePreview() {
    clearTimeout(closeTimer.current);
    closeTimer.current = setTimeout(
      () => setSelected((current) => (current?.pinned ? current : null)),
      150,
    );
  }

  const active = locations.find((location) => location.key === selected?.key);
  return (
    <div className="relative">
      <FormError error={error} />
      <div
        ref={containerRef}
        className="relative aspect-8/5 w-full max-h-100 overflow-hidden touch-none cursor-grab active:cursor-grabbing"
      >
        {map && (
          <>
            <svg
              width={map.width}
              height={map.height}
              role="img"
              aria-label="Mapa událostí"
              className="pointer-events-none"
            >
              <g transform={transform.toString()}>
                <path
                  d={map.land}
                  className="fill-neutral-3 stroke-neutral-9 [vector-effect:non-scaling-stroke]"
                  strokeWidth={0.75}
                />
                <path
                  d={map.grid}
                  className="fill-none stroke-neutral-7 [vector-effect:non-scaling-stroke]"
                  strokeWidth={0.6}
                />
              </g>
            </svg>
            {locations.map((location) => {
              const [x, y] = transform.apply(map.projection(location.coordinates)!);
              if (x < 0 || x > map.width || y < 0 || y > map.height) return null;
              return (
                <button
                  key={location.key}
                  type="button"
                  aria-label={`${location.label} · ${formatCount(location.events.length, {
                    one: 'událost',
                    few: 'události',
                    other: 'událostí',
                  })}`}
                  aria-haspopup="dialog"
                  aria-expanded={selected?.key === location.key}
                  onPointerEnter={(event) => {
                    if (event.pointerType === 'mouse')
                      open(location.key, event.currentTarget);
                  }}
                  onPointerLeave={closePreview}
                  onClick={(event) => open(location.key, event.currentTarget, true)}
                  style={{ left: x, top: y }}
                  className="absolute grid size-5 -translate-1/2 place-items-center rounded-full focus-visible:outline-2 focus-visible:outline-accent-8"
                >
                  <span
                    className={cn(
                      'rounded-full border border-neutral-1',
                      location.events.length > 1 ? 'size-3' : 'size-2',
                    )}
                    style={{ background: location.background }}
                  />
                </button>
              );
            })}
          </>
        )}
      </div>
      {map && (
        <div className="absolute right-2 top-2 flex items-center gap-1">
          <SelectField
            name="shape"
            aria-label="Pohled"
            value={view}
            onChange={onViewChange}
            options={mapViews}
            selectClassName="h-8 rounded-xl py-0"
          />
          {(
            [
              ['Přiblížit', Plus, transform.k >= 8, 2],
              ['Oddálit', Minus, transform.k <= 1, 0.5],
              ['Obnovit mapu', RotateCcw, false, null],
            ] as const
          ).map(([label, Icon, disabled, factor]) => (
            <button
              key={label}
              type="button"
              aria-label={label}
              className={buttonCls({ variant: 'outline', size: 'icon' })}
              disabled={disabled}
              onClick={() => {
                const container = select(containerRef.current!);
                if (factor === null) container.call(behavior.transform, map.initial);
                else container.call(behavior.scaleBy, factor);
              }}
            >
              <Icon />
            </button>
          ))}
        </div>
      )}
      {selected && active && (
        <Popover open onOpenChange={() => setSelected(null)}>
          <PopoverAnchor virtualRef={{ current: selected.anchor }} />
          <PopoverContent
            style={{ animation: 'none' }}
            aria-label={active.label}
            className="pointer-events-auto w-80 max-h-80 p-3 pr-9"
            sideOffset={4}
            portalClassName="pointer-events-none"
            onPointerEnter={() => clearTimeout(closeTimer.current)}
            onPointerLeave={closePreview}
            onOpenAutoFocus={(event) => {
              if (!selected.pinned) event.preventDefault();
            }}
            onCloseAutoFocus={(event) => {
              event.preventDefault();
              if (selected.pinned) selected.anchor.focus();
            }}
          >
            <p className="mb-2 font-semibold">
              {active.label}{' '}
              <span className="font-normal text-neutral-11">
                ({active.events.length})
              </span>
            </p>
            <ul className="space-y-2 text-sm">
              {active.events.map((event) => (
                <li key={event.id}>
                  <span
                    className="mr-1 inline-block size-2 rounded-full"
                    style={{ backgroundColor: eventColor(event) }}
                  />
                  <Link
                    className="text-accent-11 underline"
                    href={`/federated/${federation}/${event.externalId}`}
                    prefetch={false}
                  >
                    {formatDateOnly(event.startDate)}
                    {event.endDate && event.endDate !== event.startDate && (
                      <> – {formatDateOnly(event.endDate)}</>
                    )}
                    {event.name && event.name !== active.label && (
                      <span className="block">{event.name}</span>
                    )}
                  </Link>
                  <div className="text-xs text-neutral-11">
                    Soutěže: {event.competitions.totalCount}
                  </div>
                </li>
              ))}
            </ul>
          </PopoverContent>
        </Popover>
      )}
    </div>
  );
}

function eventColor(event: FederationMapEventFragment) {
  return eventMonthColors[Number(event.startDate.slice(5, 7)) - 1];
}

function eventLocations(events: FederationMapEventFragment[]) {
  return groups(
    events
      .filter((e) => e.venueLat !== null && e.venueLng !== null)
      .toSorted((a, b) => a.startDate.localeCompare(b.startDate)),
    (e) => `${e.venueLng},${e.venueLat}`,
  ).map(([key, events]) => {
    const first = events[0]!;
    const colors = events.map(
      (e, i) =>
        `${eventColor(e)} ${(i * 100) / events.length}% ${((i + 1) * 100) / events.length}%`,
    );
    return {
      key,
      events,
      coordinates: [first.venueLng!, first.venueLat!] as [number, number],
      label:
        [first.city || first.location, first.country].filter(Boolean).join(', ') ||
        first.name ||
        first.externalId,
      background: `conic-gradient(${colors.join(',')})`,
    };
  });
}

function projectMap(
  geography: ExtendedFeatureCollection,
  view: MapView,
  width: number,
  height: number,
) {
  const projection = (
    view === 'czechia' ? geoAzimuthalEquidistant().rotate([-20, 0]) : geoRobinson()
  )
    .fitExtent(
      [
        [5, 5],
        [width - 5, height - 5],
      ],
      view === 'czechia' ? geography : { type: 'Sphere' },
    )
    .clipExtent([
      [0, 0],
      [width, height],
    ]);
  const path = geoPath(projection);
  let initial = zoomIdentity;
  if (view === 'europe') {
    const [[x0, y0], [x1, y1]] = path.bounds(europe);
    const scale = Math.min((width - 10) / (x1 - x0), (height - 10) / (y1 - y0));
    initial = zoomIdentity
      .translate(width / 2, height / 2)
      .scale(scale)
      .translate(-(x0 + x1) / 2, -(y0 + y1) / 2);
  }
  return {
    width,
    height,
    projection,
    initial,
    land: path(geography) ?? undefined,
    grid: path(graticule) ?? undefined,
  };
}
