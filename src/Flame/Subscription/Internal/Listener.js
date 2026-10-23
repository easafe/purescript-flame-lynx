//this is not ideal, e.g. if the library is loaded twice
let applicationIds = new Map();

export function checkApplicationId_(id) {
    if (applicationIds.get(id)?.registered)
        throw `Error mounting application: id ${id} already registered!`;

    let application = applicationIds.get(id) ?? { messages: [] };

    application.registered = true;
    applicationIds.set(id, application);
}

export function registerApplicationId_(id) {
    let application = applicationIds.get(id),
        messages = application.messages;

    application.messages = null;

    return messages;
}

export function isApplicationInitialized_(id) {
    return applicationIds.get(id)?.messages === null;
}

export function queueMessage_(id, message) {
    let application = applicationIds.get(id) ?? { registered: false, messages: [] };

    application.messages.push(message);
    applicationIds.set(id, application);
}
