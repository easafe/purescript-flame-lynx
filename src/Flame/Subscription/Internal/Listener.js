//this is not ideal, e.g. if the library is loaded twice
let applicationIds = new Map();

export function checkApplicationId_(id) {
    if (applicationIds.has(id))
        throw `Error mounting application: id ${id} already registered!`;

    applicationIds.set(id, false);
}

export function registerApplicationId_(id) {
    applicationIds.set(id, true);
}

export function isApplicationRegistered_(id) {
    return applicationIds.get(id) === true;
}
