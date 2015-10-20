#!/bin/bash

fsharpi --lib:Libs/Infers.Core/bin/Debug  \
        --lib:Libs/Infers/bin/Debug       \
        --lib:Libs/Infers.Rep/bin/Debug   \
        --lib:Libs/Infers.Toys/bin/Debug  \
        --use:Infers.fsx
