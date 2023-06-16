import { Resource, Event } from "./types"

export type ResourceGrouper = ReturnType<typeof makeGrouper>;

export default function makeGrouper(resources: Resource[]) {
  return {
    map<T>(fn: (x: [Resource | undefined, number], ix: number) => T): T[] {
      if (!resources) return [fn([undefined, -1], 0)]
      return resources.map((resource, idx) => fn([resource, resource.resourceId], idx))
    },

    groupEvents(events: Event[]): Map<number|undefined, Event[]> {
      const eventsByResource = new Map()

      if (!resources) {
        // Return all events if resources are not provided
        eventsByResource.set(undefined, events)
        return eventsByResource
      }

      events.forEach((event) => {
        const id = event.resourceId || undefined
        if (Array.isArray(id)) {
          id.forEach((item) => {
            let resourceEvents = eventsByResource.get(item) || []
            resourceEvents.push(event)
            eventsByResource.set(item, resourceEvents)
          })
        } else {
          let resourceEvents = eventsByResource.get(id) || []
          resourceEvents.push(event)
          eventsByResource.set(id, resourceEvents)
        }
      })
      return eventsByResource
    },
  }
}
